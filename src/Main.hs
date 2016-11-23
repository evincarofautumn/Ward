{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception, throw)
import Data.Foldable (foldlM, traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid -- *
import Data.Set (Set)
import Data.Traversable (forM)
import Data.Typeable (Typeable)
import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(..))
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST -- *
import Language.C.System.GCC (newGCC)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint (render)
import Types
import qualified Args
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = do
  args <- Args.parse
  let temporaryDirectory = Nothing
  let preprocessor = newGCC $ Args.preprocessorPath args
  parseResults <- forM (Args.translationUnitPaths args)
    $ parseCFile preprocessor temporaryDirectory
    $ Args.preprocessorFlags args
  case sequence parseResults of
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError
    Right translationUnits -> do
      let
        -- FIXME: Avoid collisions with static definitions.
        translationUnit = joinTranslationUnits translationUnits
        implicitPermissions = Set.fromList
          [ Permission $ Text.pack permission
          | Args.GrantFlag permission <- Args.flags args
          ]
        global = globalContextFromTranslationUnit
          implicitPermissions translationUnit
        symbolTable = globalPermissionActions global
      mapM_ (\ (Ident name _ _, permissions)
        -> putStrLn $ show name ++ ": " ++ show (Set.toList permissions))
        $ Map.toList symbolTable
      print $ Map.size $ globalFunctions global
      checkFunctions global

joinTranslationUnits :: [CTranslUnit] -> CTranslUnit
joinTranslationUnits translationUnits@(CTranslUnit _ firstLocation : _)
  = CTranslUnit
    (concat
      [ externalDeclarations
      | CTranslUnit externalDeclarations _ <- translationUnits
      ])
    firstLocation
joinTranslationUnits [] = error "joinTranslationUnits: empty input"

warn :: String -> IO ()
warn = putStrLn

checkFunctions :: GlobalContext -> IO ()
checkFunctions global
  = traverse_ (checkFunction mempty) $ globalFunctions global
  where
    checkFunction :: LocalContext -> CFunDef -> IO ()
    checkFunction local (CFunDef specifiers
      declarator@(CDeclr (Just ident) _ _ _ _) parameters body _) = do
      putStrLn $ render $ pretty declarator
      let
        permissionActions = fromMaybe mempty
          (Map.lookup ident $ globalPermissionActions global)
          <> extractPermissionActions
            [attr | CTypeQual (CAttrQual attr) <- specifiers]
      putStrLn $ "Granting: " ++ show permissionActions
      -- Grant/waive permissions locally.
      local' <- foldlM applyPreAction local permissionActions
      local'' <- checkStatement local' body
      -- Verify postconditions.
      local''' <- foldlM (applyPermissionAction NoReason) local'' permissionActions
      -- TODO: check that all added permissions (inferred \ declared) have been
      -- granted, and all dropped permissions (declared \ inferred) have been
      -- revoked.
      putStrLn $ replicate 40 '-'
    checkFunction _ _ = return ()

    -- It would be nicer for pipelining if the check* functions took
    -- LocalContext last, but taking it first is convenient for folding.

    checkStatement :: LocalContext -> CStat -> IO LocalContext
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
      CIf condition true mFalse _ -> do
        local' <- checkExpression local condition
        localTrue <- checkStatement local' true
        localFalse <- foldlM checkStatement local' mFalse
        unifyBranches local' localTrue localFalse

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

    checkBlockItem :: LocalContext -> CBlockItem -> IO LocalContext
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

    checkExpression :: LocalContext -> CExpr -> IO LocalContext
    checkExpression local = \ case

      -- a, b, ...
      CComma expressions _ -> do
        foldlM checkExpression local expressions

      -- a [op]= b
      CAssign _operator a b _ -> do
        local' <- checkExpression local a
        checkExpression local' b

      -- a ? b : c
      CCond a mb c _ -> do
        local' <- checkExpression local a
        localTrue <- foldlM checkExpression local' mb
        localFalse <- checkExpression local' c
        unifyBranches local' localTrue localFalse

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
      CCall function arguments _ -> do
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
                foldlM (applyPermissionAction (BecauseCall ident)) local'' permissionActions
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
            warn $ concat
              [ "ward warning: indirect call not handled in: "
              , render $ pretty function
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

    checkInitializerList :: LocalContext -> CInitList -> IO LocalContext
    checkInitializerList = foldlM $ \ local (_partDesignators, initializer)
      -> checkInitializer local initializer

    checkInitializer :: LocalContext -> CInit -> IO LocalContext
    checkInitializer local = \ case
      CInitExpr expression _ -> checkExpression local expression
      CInitList initializers _ -> checkInitializerList local initializers

    applyPreAction :: LocalContext -> PermissionAction -> IO LocalContext
    applyPreAction local (PermissionAction action permission) = case action of
      Need -> applyPermissionAction NoReason local
        $ PermissionAction Grant permission
      Grant -> return local
      Revoke -> return local  -- FIXME: Not sure if this is correct.
      Waive -> return local
      {-
        applyPermissionAction NoReason local
          $ PermissionAction Revoke permission
      -}

    applyPermissionAction :: Reason -> LocalContext -> PermissionAction -> IO LocalContext
    applyPermissionAction reason local (PermissionAction action permission)
      = case action of

        Need
          | permission `Set.member` localPermissionState local
          -> return local
          | otherwise -> do
            warn $ concat
              [ "ward error: because of "
              , show reason
              , ", need permission '"
              , show permission
              , "' not present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

        Grant
          | permission `Set.member` localPermissionState local -> do
{-
            warn $ concat
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
          | permission `Set.member` localPermissionState local
          -> return local { localPermissionState = Set.delete permission
            $ localPermissionState local }
          | otherwise -> do
            warn $ concat
              [ "ward error: revoking permission '"
              , show permission
              , "' not present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

        -- Local waiving of permissions has no effect on the outer context.
        Waive -> return local

-- | Verifies that two local contexts match, using a prior context to produce
-- detailed warnings in the event of a mismatch.
unifyBranches
  :: LocalContext  -- ^ Prior context.
  -> LocalContext  -- ^ Context from first branch.
  -> LocalContext  -- ^ Context from second branch.
  -> IO LocalContext
unifyBranches prior true false
  | localPermissionState true == localPermissionState false = return true
  | otherwise = do
    let union = true <> false
    warn $ concat
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

data GlobalContext = GlobalContext
  { globalPermissionActions :: !(Map Ident (Set PermissionAction))
  , globalFunctions :: !(Map Ident CFunDef)
  }

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

data LocalContext = LocalContext
  { localPermissionState :: !(Set Permission)
  -- , localVariables :: !(Map Ident [PermissionAction])
  } deriving (Eq)

instance Monoid LocalContext where
  mempty = LocalContext
    { localPermissionState = mempty
    }
  mappend a b = LocalContext
    { localPermissionState = localPermissionState a <> localPermissionState b
    }

globalContextFromTranslationUnit
  :: Set Permission -> CTranslUnit -> GlobalContext
globalContextFromTranslationUnit
  implicitPermissions (CTranslUnit externalDeclarations _)
  = foldr (insertTopLevelElement implicitPermissions)
    mempty externalDeclarations

data WardException
  = UnknownPermissionActionException String
  deriving (Show, Typeable)

instance Exception WardException

insertTopLevelElement
  :: Set Permission -> CExtDecl -> GlobalContext -> GlobalContext
insertTopLevelElement implicitPermissions element global = case element of

  -- For an external declaration, record the permission actions in the context.
  CDeclExt (CDecl specifiers fullDeclarators _) -> global
    { globalPermissionActions = foldr (uncurry (mapInsertWithOrDefault combine))
      (globalPermissionActions global) identPermissions }
    where
      declaratorPermissions = extractDeclaratorPermissionActions fullDeclarators
      specifierPermissions = extractPermissionActions
        [attr | CTypeQual (CAttrQual attr) <- specifiers]
      identPermissions = declaratorPermissions
        ++ [(ident, specifierPermissions) | ident <- map fst declaratorPermissions]

  -- For a function definition, record the function body in the context.
  -- TODO: parse attributes from parameters
  CFDefExt definition@(CFunDef specifiers
    (CDeclr (Just ident) _ _ _ _) parameters _body _) -> global
      { globalPermissionActions = mapInsertWithOrDefault combine
        ident specifierPermissions $ globalPermissionActions global
      , globalFunctions = Map.insert ident definition
        $ globalFunctions global
      }
    where
      specifierPermissions = extractPermissionActions
        [attr | CTypeQual (CAttrQual attr) <- specifiers]

  CFDefExt{} -> global  -- TODO: warn?
  CAsmExt{} -> global  -- TODO: warn?
  where
    combine
      :: Set PermissionAction
      -> Maybe (Set PermissionAction)
      -> Set PermissionAction
    combine new mOld = newGranted <> case mOld of
      Nothing -> Set.map (PermissionAction Need) implicitPermissions Set.\\ newWaived
      Just old -> old Set.\\ newWaived
      where
        newGranted = Set.fromList
          [ permissionAction
          | permissionAction@(PermissionAction action _) <- Set.toList new
          , action /= Waive
          ]
        newWaived = Set.fromList
          [ PermissionAction Need permission
          | PermissionAction Waive permission <- Set.toList new
          ]

mapInsertWithOrDefault
  :: (Ord k) => (v -> Maybe v -> v) -> k -> v -> Map k v -> Map k v
mapInsertWithOrDefault combine key new m
  = Map.insert key (combine new (Map.lookup key m)) m

extractDeclaratorPermissionActions
  :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> [(Ident, Set PermissionAction)]
extractDeclaratorPermissionActions = foldr go []
  where
    -- TODO: Do something with derived declarators?
    go (Just (CDeclr (Just ident) derived _ attributes _), _, _) acc
      | Set.null permissionActions = acc
      | otherwise = (ident, permissionActions) : acc
      where
        permissionActions = extractPermissionActions attributes
    go _ acc = acc

extractPermissionActions :: [CAttr] -> Set PermissionAction
extractPermissionActions attributes = Set.fromList $ do
  CAttr (Ident "permission" _ _) expressions _ <- attributes
  CCall (CVar (Ident actionName _ _) _) permissions _ <- expressions
  CVar (Ident permission _ _) _ <- permissions
  action <- case actionName of
    "need" -> return Need
    "grant" -> return Grant
    "revoke" -> return Revoke
    "waive" -> return Waive
    _ -> throw $ UnknownPermissionActionException actionName
  return $ PermissionAction action $ Permission $ Text.pack permission
