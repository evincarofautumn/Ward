{-# LANGUAGE LambdaCase #-}

module Check
  ( translationUnits
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.List -- *
import Data.Foldable (foldlM, foldrM, traverse_)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid -- *
import Data.Set (Set)
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Node (NodeInfo)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST -- *
import Language.C.Syntax.Constants -- *
import Text.PrettyPrint (render)
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

data GlobalContext = GlobalContext
  { globalPermissionActions :: !(Map Ident (Set PermissionAction))
  , globalFunctions :: !(Map Ident CFunDef)
  }

data LocalContext = LocalContext
  { localPermissionState :: !(Set Permission)
  , localVarPermissions :: !(Map String (Set Permission))
  } deriving (Eq)

instance Monoid GlobalContext where
  mempty = GlobalContext
    { globalPermissionActions = mempty
    , globalFunctions = mempty
    }
  mappend a b = GlobalContext
    { globalPermissionActions
      = globalPermissionActions a <> globalPermissionActions b
    , globalFunctions = globalFunctions a <> globalFunctions b
    }

instance Monoid LocalContext where
  mempty = LocalContext
    { localPermissionState = mempty
    , localVarPermissions = mempty
    }
  mappend a b = LocalContext
    { localPermissionState = localPermissionState a <> localPermissionState b
    -- TODO: Verify that merging with simple set union is correct.
    , localVarPermissions = Map.unionWith (<>)
      (localVarPermissions a) (localVarPermissions b)
    }

translationUnits :: [CTranslUnit] -> Set Permission -> Logger ()
translationUnits tus implicitPermissions = do
  -- FIXME: Avoid collisions with static definitions.
  let translationUnit = joinTranslationUnits tus
  global <- globalContextFromTranslationUnit
    implicitPermissions translationUnit
  let symbolTable = globalPermissionActions global
  checkFunctions global

joinTranslationUnits :: [CTranslUnit] -> CTranslUnit
joinTranslationUnits tus@(CTranslUnit _ firstLocation : _)
  = CTranslUnit
    (concat
      [ externalDeclarations
      | CTranslUnit externalDeclarations _ <- tus
      ])
    firstLocation
joinTranslationUnits [] = error "joinTranslationUnits: empty input"

checkFunctions :: GlobalContext -> Logger ()
checkFunctions global
  = traverse_ (checkFunction mempty) $ globalFunctions global
  where
    checkFunction :: LocalContext -> CFunDef -> Logger ()
    checkFunction local (CFunDef specifiers
      declarator@(CDeclr (Just ident@(Ident name _ pos)) _ _ _ _)
      parameters body _) = do
      let
        parameterNames =
          [ Just parameterName
          | CDecl _ parameterDeclarations _ <- parameters
          , (Just (CDeclr (Just (Ident parameterName _ _)) _ _ _ _), _, _)
            <- parameterDeclarations
          ]
      extractedActions <- extractPermissionActions
        [attr | CTypeQual (CAttrQual attr) <- specifiers]
      let
        permissionActions = fromMaybe mempty
          (Map.lookup ident $ globalPermissionActions global)
          <> extractedActions
      record $ Note pos $ Text.pack $ concat
        [ "checking '"
        , name
        , "'"
        ]
      -- Grant/waive permissions locally.
      local' <- foldlM (applyPreAction pos parameterNames)
        local permissionActions
      local'' <- checkStatement local' body
      -- Verify postconditions.
      local''' <- foldlM (applyPermissionAction (NoReason pos) parameterNames)
        local'' permissionActions
      -- TODO: check that all added permissions (inferred \ declared) have been
      -- granted, and all dropped permissions (declared \ inferred) have been
      -- revoked.
      return ()
    checkFunction _ _ = return ()

    -- It would be nicer for pipelining if the check* functions took
    -- LocalContext last, but taking it first is convenient for folding.

    checkStatement :: LocalContext -> CStat -> Logger LocalContext
    checkStatement local = \ case

      -- label: stmt
      -- TODO: Accumulate labels for gotos?
      CLabel _label statement _attributes _ -> do
        checkStatement local statement

      -- case expr: stmt
      -- Should only be encountered when traversing switch. Starts a new branch.
      CCase _expression statement _ -> do
        checkStatement local statement

      -- case lower ... upper: stmt
      -- GNU extension, maybe don't bother to support?
      CCases _lower _upper statement _ -> do
        checkStatement local statement

      -- default: stmt
      -- Switch only; starts a new branch.
      CDefault statement _ -> do
        checkStatement local statement

      -- ;
      -- No change.
      CExpr Nothing _ -> do
        return local

      -- expr;
      CExpr (Just expression) _ -> checkExpression local expression

      -- { block-item* }
      -- Check each block-item in order.
      CCompound _localLabels blockItems _ -> do
        foldlM checkBlockItem local blockItems

      -- if (expr) stmt (else stmt}?
      -- Check true and false branches and take their union.
      CIf condition true mFalse pos -> do
        local' <- checkExpression local condition
        localTrue <- checkStatement local' true
        localFalse <- foldlM checkStatement local' mFalse
        unifyBranches pos local' localTrue localFalse

      -- switch (expr) body
      -- Traverse all branches in body and take their union.
      CSwitch scrutinee body _ -> do
        local' <- checkExpression local scrutinee
        checkStatement local' body

      -- while (expr) stmt / do stmt while (expr);
      -- Treat as if (expr) stmt else ;
      CWhile condition body isDoWhile _ -> do
        if isDoWhile
          then do
            local' <- checkStatement local body
            checkExpression local' condition
          else do
            local' <- checkExpression local condition
            checkStatement local' body

      -- for (init; expr; expr) stmt
      -- Treat as init; while (expr) { stmt; expr; } ...maybe?
      CFor initializer{-:: Either (Maybe (CExpression a)) (CDeclaration a) -}
        mCondition -- Maybe (CExpression a)
        mStep -- Maybe (CExpression a)
        body
        _ -> do
        local' <- case initializer of
          Left mExpression -> foldlM checkExpression local mExpression
          Right declaration -> return local  -- TODO: check initializer
        local'' <- foldlM checkExpression local' mCondition
        local''' <- checkStatement local'' body
        foldlM checkExpression local''' mStep

      -- goto label;
      -- Do something magic with control flow? Or just bail out.
      CGoto _label _ -> return local

      -- goto expr;
      CGotoPtr expression _ -> do
        checkExpression local expression

      -- continue;
      -- Unify remainder of loop with whole loop?
      CCont _ -> return local

      -- break;
      -- Unify foregoing statements in loop with whole loop?
      CBreak _ -> return local

      -- return expr?;
      -- Ditto break, except for functions, not loops?
      CReturn mExpression _ -> do
        foldlM checkExpression local mExpression

      -- No idea what to do with assembly statements.
      CAsm{} -> return local

    checkBlockItem :: LocalContext -> CBlockItem -> Logger LocalContext
    checkBlockItem local = \ case
      CBlockStmt statement -> checkStatement local statement
      CBlockDecl (CDecl _specifiers declarations _)
        -> foldlM checkInitializer local
          [initializer | (_, Just initializer, _) <- declarations]
      -- GNU nested function
      CNestedFunDef{} -> return local

    -- This assumes a left-to-right evaluation order for binary expressions and
    -- function arguments, which is standard-compliant but not necessarily the
    -- same as what your compiler does.

    checkExpression :: LocalContext -> CExpr -> Logger LocalContext
    checkExpression local = \ case

      -- a, b, ...
      CComma expressions _ -> do
        foldlM checkExpression local expressions

      -- a [op]= b
      CAssign _operator a b _ -> do
        local' <- checkExpression local a
        checkExpression local' b

      -- a ? b : c
      CCond a mb c pos -> do
        local' <- checkExpression local a
        localTrue <- foldlM checkExpression local' mb
        localFalse <- checkExpression local' c
        unifyBranches pos local' localTrue localFalse

      -- a op b
      CBinary _operator a b _ -> do
        local' <- checkExpression local a
        checkExpression local' b

      -- (type)expr
      CCast _declaration expression _ -> do
        checkExpression local expression

      -- op a
      CUnary _operator a _ -> do
        checkExpression local a

      -- sizeof expr
      CSizeofExpr{} -> return local

      -- sizeof (type)
      CSizeofType{} -> return local

      -- alignof expr
      CAlignofExpr{} -> return local

      -- alignof (type)
      CAlignofType{} -> return local

      -- Real(a)
      CComplexReal expression _ -> do
        checkExpression local expression

      -- Imag(a)
      CComplexImag expression _ -> do
        checkExpression local expression

      -- a[b]
      CIndex a b _ -> do
        local' <- checkExpression local a
        checkExpression local' b

      -- f(a, b, ...)
      CCall function arguments callPos -> do
        let
          argumentNames =
            [ case argument of
              CVar (Ident argumentName _ _) _ -> Just argumentName
              _ -> Nothing
            | argument <- arguments
            ]
        local' <- checkExpression local function
        local'' <- foldlM checkExpression local' arguments
        case function of
          CVar ident _
            -> case Map.lookup ident $ globalPermissionActions global of
              Just permissionActions -> do
{-
                warn $ concat
                  [ "ward note: applying actions: "
                  , show $ Set.toList permissionActions
                  ]
-}
                foldlM (applyPermissionAction (BecauseCall ident) argumentNames)
                  local'' permissionActions
              Nothing -> do
{-
                warn $ concat
                  [ "ward warning: calling function '"
                  , name
                  , "' but can't find permissions for it"
                  ]
-}
                return local''
          _ -> do
            record $ Warning callPos $ Text.pack $ concat
              [ "indirect call '"
              , render $ pretty function
              , "' not handled"
              ]
            return local''

      -- expr.ident
      -- expr->ident
      CMember expression _ident _isDeref _ -> do
        checkExpression local expression

      -- x
      -- TODO: track types of local variables
      CVar{} -> return local

      -- 0 '0' 0.0 ""
      CConst{} -> return local

      -- (type){ ... }
      CCompoundLit _declaration initializers _ -> do
        checkInitializerList local initializers

      -- GNU ({ ... })
      CStatExpr statement _ -> do
        checkStatement local statement

      -- GNU &&label
      CLabAddrExpr{} -> return local

      -- GNU builtins: va_arg, offsetof, __builtin_types_compatible_p
      CBuiltinExpr{} -> return local

    checkInitializerList :: LocalContext -> CInitList -> Logger LocalContext
    checkInitializerList = foldlM $ \ local (_partDesignators, initializer)
      -> checkInitializer local initializer

    checkInitializer :: LocalContext -> CInit -> Logger LocalContext
    checkInitializer local = \ case
      CInitExpr expression _ -> checkExpression local expression
      CInitList initializers _ -> checkInitializerList local initializers

    applyPreAction
      :: NodeInfo
      -> [Maybe String]
      -> LocalContext
      -> PermissionAction
      -> Logger LocalContext
    applyPreAction pos argumentNames local
      (PermissionAction action mSubject permission) = case action of
        Need -> applyPermissionAction (NoReason pos) argumentNames local
          $ PermissionAction Grant mSubject permission
        Deny -> return local
        Grant -> return local
        Revoke -> return local  -- FIXME: Not sure if this is correct.
        Waive -> return local
        {-
          applyPermissionAction NoReason local
            $ PermissionAction Revoke permission
        -}

    applyPermissionAction
      :: Reason
      -> [Maybe String]
      -> LocalContext
      -> PermissionAction
      -> Logger LocalContext
    applyPermissionAction reason argumentNames local
      (PermissionAction action mSubject permission)
      = case action of

        Need
          | Just subject <- mSubject
          -- FIXME: Avoid fromJust.
          -> let
            argumentName = fromJust (argumentNames !! subject)
            in case Map.lookup argumentName $ localVarPermissions local of
              Just varPermissions

                -- The variable has the permission: do nothing.
                | permission `Set.member` varPermissions -> return local

                -- The variable has permissions, but not the required ones:
                -- report it.
                | otherwise -> do
                  record $ Error (reasonPos reason) $ Text.pack $ concat
                    [ "because of "
                    , show reason
                    , ", need permission '"
                    , show permission
                    , "' for variable '"
                    -- FIXME: Avoid fromJust.
                    , fromJust $ argumentNames !! subject
                    , "' not present in context "
                    , show $ Set.toList varPermissions
                    ]
                  return local

              -- The variable does not have permissions: report it.
              Nothing -> do
                record $ Error (reasonPos reason) $ Text.pack $ concat
                  [ "because of "
                  , show reason
                  , ", need permission '"
                  , show permission
                  , "' for variable '"
                  -- FIXME: Avoid fromJust.
                  , fromJust $ argumentNames !! subject
                  , "' not present in context []"
                  ]
                return local

          -- No subject, and permission in the local context: do nothing.
          | permission `Set.member` localPermissionState local
          -> return local

          -- No subject, and permission not present: report it.
          | otherwise -> do
            record $ Error (reasonPos reason) $ Text.pack $ concat
              [ "because of "
              , show reason
              , ", need permission '"
              , show permission
              , "' not present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

        Deny
          | Just subject <- mSubject
          -- FIXME: Avoid fromJust.
          -> let
            argumentName = fromJust (argumentNames !! subject)
            in case Map.lookup argumentName $ localVarPermissions local of
              Just varPermissions

                -- The variable has the disallowed permission: report it.
                | permission `Set.member` varPermissions -> do
                  record $ Error (reasonPos reason) $ Text.pack $ concat
                    [ "because of "
                    , show reason
                    , ", denying disallowed permission '"
                    , show permission
                    , "' for variable '"
                    -- FIXME: Avoid fromJust.
                    , fromJust $ argumentNames !! subject
                    , "' present in context "
                    , show $ Set.toList varPermissions
                    ]
                  return local

              -- The variable does not have permissions, or the permissions it
              -- has don't include the disallowed one: do nothing.
              _ -> return local

          -- No subject, and permission in the local context: report it.
          | permission `Set.member` localPermissionState local -> do
            record $ Error (reasonPos reason) $ Text.pack $ concat
              [ "because of "
              , show reason
              , ", denying disallowed permission '"
              , show permission
              , "' present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

          -- No subject, and permission not present: do nothing.
          | otherwise -> return local

        Grant
          -- When granting a permission to a particular subject, the name of the
          -- subject is first looked up by its index in the arguments of a call
          -- or parameters of a definition:
          --
          --     int lock_file (int fd)
          --       __attribute__ ((permission (grant (flocked (0)))));
          --
          -- At call sites:
          --
          --     lock_file (my_fd);
          --     // grant (flocked (0)) -> my_fd : [flocked]
          --
          -- In definitions:
          --
          --     int lock_file (int fd)
          --     {
          --       // grant (flocked (0)) -> fd : [flocked]
          --     }
          --
          | Just subject <- mSubject
          -> return local
            -- FIXME: (!!) can throw out-of-bounds.
            -- FIXME: Avoid fromJust.
            -- TODO: Verify that merging with simple set union is correct.
            { localVarPermissions = Map.insertWith (<>)
              (fromJust (argumentNames !! subject)) (Set.singleton permission)
              $ localVarPermissions local }

          -- If there is no subject, we grant the permission to all local calls.
          | permission `Set.member` localPermissionState local -> do
{-
            putStrLn $ concat
              [ "ward warning: granting permission '"
              , show permission
              , "' already present in context "
              , show $ Set.toList $ localPermissionState local
              ]
-}
            return local
          | otherwise -> return local
            { localPermissionState = Set.insert permission
              $ localPermissionState local }

        Revoke
          | Just subject <- mSubject
          -> let
            argumentName = fromJust (argumentNames !! subject)
            in return local
              -- FIXME: (!!) can throw out-of-bounds.
              -- FIXME: Avoid fromJust.
              -- FIXME: Report errors.
              { localVarPermissions =
                case Map.lookup argumentName $ localVarPermissions local of
                  Just existing -> Map.insert argumentName
                    (Set.delete permission existing)
                    $ localVarPermissions local
                  Nothing -> localVarPermissions local
              }

          | permission `Set.member` localPermissionState local
          -> return local { localPermissionState = Set.delete permission
            $ localPermissionState local }

          | otherwise -> do
            record $ Error (reasonPos reason) $ Text.pack $ concat
              [ "revoking permission '"
              , show permission
              , "' not present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

        -- Local waiving of permissions has no effect on the outer context.
        -- Implicitly granted permissions can't have a subject, so we don't need
        -- to check for one here.
        Waive -> return local

-- | Verifies that two local contexts match, using a prior context to produce
-- detailed warnings in the event of a mismatch.
unifyBranches
  :: NodeInfo      -- ^ Source position.
  -> LocalContext  -- ^ Prior context.
  -> LocalContext  -- ^ Context from first branch.
  -> LocalContext  -- ^ Context from second branch.
  -> Logger LocalContext
unifyBranches pos prior true false
  | localPermissionState true == localPermissionState false = return true
  | otherwise = do
    let union = true <> false
    record $ Warning pos $ Text.pack $ concat
      [ "ward warning: "
      , show $ localPermissionState prior
      , " -> "
      , show $ localPermissionState true
      , " /= "
      , show $ localPermissionState false
      , "\n"
      , "ward warning: unsafely assuming their union "
      , show $ localPermissionState union
      ]
    return union

globalContextFromTranslationUnit
  :: Set Permission -> CTranslUnit -> Logger GlobalContext
globalContextFromTranslationUnit
  implicitPermissions (CTranslUnit externalDeclarations _)
  = foldrM (insertTopLevelElement implicitPermissions)
    mempty externalDeclarations

insertTopLevelElement
  :: Set Permission -> CExtDecl -> GlobalContext -> Logger GlobalContext
insertTopLevelElement implicitPermissions element global = case element of

  -- For an external declaration, record the permission actions in the context.
  CDeclExt (CDecl specifiers fullDeclarators _) -> do
    declaratorPermissions <- extractDeclaratorPermissionActions fullDeclarators
    specifierPermissions <- extractPermissionActions
      [attr | CTypeQual (CAttrQual attr) <- specifiers]
    let
      identPermissions = declaratorPermissions ++
        [ (ident, specifierPermissions)
        | ident <- map fst declaratorPermissions
        ]
    return global
      { globalPermissionActions = foldr
        (uncurry (mapInsertWithOrDefault combine))
        (globalPermissionActions global) identPermissions }

  -- For a function definition, record the function body in the context.
  -- TODO: parse attributes from parameters
  CFDefExt definition@(CFunDef specifiers
    (CDeclr (Just ident) _ _ _ _) parameters _body _) -> do
      specifierPermissions <- extractPermissionActions
        [attr | CTypeQual (CAttrQual attr) <- specifiers]
      return global
        { globalPermissionActions = mapInsertWithOrDefault combine
          ident specifierPermissions $ globalPermissionActions global
        , globalFunctions = Map.insert ident definition
          $ globalFunctions global
        }

  CFDefExt{} -> return global  -- TODO: warn?
  CAsmExt{} -> return global  -- TODO: warn?
  where
    combine
      :: Set PermissionAction
      -> Maybe (Set PermissionAction)
      -> Set PermissionAction
    combine new mOld = newGranted <> case mOld of
      Nothing -> Set.map (PermissionAction Need Nothing) implicitPermissions
        Set.\\ newWaived
      Just old -> old Set.\\ newWaived
      where
        newGranted = Set.fromList
          [ permissionAction
          | permissionAction@(PermissionAction action _ _) <- Set.toList new
          , action /= Waive
          ]
        newWaived = Set.fromList
          [ PermissionAction Need mSubject permission
          | PermissionAction Waive mSubject permission <- Set.toList new
          ]

mapInsertWithOrDefault
  :: (Ord k) => (v -> Maybe v -> v) -> k -> v -> Map k v -> Map k v
mapInsertWithOrDefault combine key new m
  = Map.insert key (combine new (Map.lookup key m)) m

extractDeclaratorPermissionActions
  :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)]
  -> Logger [(Ident, Set PermissionAction)]
extractDeclaratorPermissionActions = foldrM go []
  where
    -- TODO: Do something with derived declarators?
    go (Just (CDeclr (Just ident) derived _ attributes _), _, _) acc = do
      permissionActions <- extractPermissionActions attributes
      return $ if Set.null permissionActions
        then acc
        else (ident, permissionActions) : acc
    go _ acc = return acc

extractPermissionActions :: [CAttr] -> Logger (Set PermissionAction)
extractPermissionActions attributes = fmap Set.fromList . runListT $ do
  CAttr (Ident "permission" _ _) expressions _ <- select attributes
  CCall (CVar (Ident actionName _ _) _) permissions pos <- select expressions
  permissionSpec <- select permissions
  (permission, mSubject) <- case permissionSpec of
    CVar (Ident permission _ _) _ -> return (permission, Nothing)
    CCall
      (CVar (Ident permission _ _) _)
      [CConst (CIntConst (CInteger subject _ _) _)]
      _
      -> return (permission, Just (fromInteger subject))
    other -> do
      lift $ record $ Error pos $ Text.pack $ concat
        [ "malformed permission specifier '"
        , render $ pretty other
        , "'; ignoring"
        ]
      mzero
  action <- case actionName of
    "need" -> return Need
    "deny" -> return Deny
    "grant" -> return Grant
    "revoke" -> return Revoke
    "waive" -> return Waive
    _ -> do
      lift $ record $ Error pos $ Text.pack $ concat
        [ "unknown permission action '"
        , actionName
        , "'; ignoring"
        ]
      mzero
  return $ PermissionAction action mSubject
    $ Permission $ Text.pack permission

select :: (Monad m) => [a] -> ListT m a
select = ListT . return
