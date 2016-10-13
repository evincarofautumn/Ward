{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception, throw)
import Data.Foldable (foldlM, traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid -- *
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Typeable (Typeable)
import GHC.Exts (IsString)
import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(..))
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST -- *
import Language.C.System.GCC (newGCC)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint (render)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = do
  let temporaryDirectory = Nothing
  args <- getArgs
  (preprocessorPath, filePaths, preprocessorFlags) <- case args of
    [] -> usage
    [_] -> usage
    pp : args -> case break (== "--") args of
      (paths, flags) -> return (pp, paths, drop 1 flags)
  let preprocessor = newGCC preprocessorPath
  results <- forM filePaths $ parseCFile preprocessor temporaryDirectory
    (defaultPreprocessorFlags ++ preprocessorFlags)
  case sequence results of
    Left parseError -> do
      putStrLn "Parse error:"
      print parseError
    Right translationUnits -> do
      let translationUnit = joinTranslationUnits translationUnits
      let global = globalContextFromTranslationUnit translationUnit
      let symbolTable = globalPermissionActions global
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
      | CTranslUnit externalDeclarations nodeInfo <- translationUnits
      ])
    firstLocation
joinTranslationUnits [] = error "joinTranslationUnits: empty input"

usage :: IO a
usage = do
  warn "Usage: ward <preprocessor> <sources> [-- <preprocessor flags>]"
  exitFailure

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
      -- Grant needed permissions locally.
      local' <- foldlM applyPreAction local permissionActions
      local'' <- checkStatement local' body
      -- Verify postconditions.
      local''' <- foldlM (applyPermissionAction NoReason) local'' permissionActions
      -- TODO: check that all added permissions (inferred \ declared) have been
      -- granted, and all dropped permissions (declared \ inferred) have been
      -- revoked.
      putStrLn $ replicate 40 '-'
    checkFunction _ _ = noop

    -- It would be nicer for pipelining if the check* functions took
    -- LocalContext last, but taking it first is convenient for folding.

    checkStatement :: LocalContext -> CStat -> IO LocalContext
    checkStatement local = \ case

      -- label: stmt
      -- TODO: Accumulate labels for gotos?
      CLabel label statement attributes _ -> do
        checkStatement local statement

      -- case expr: stmt
      -- Should only be encountered when traversing switch. Starts a new branch.
      CCase _expression statement _ -> do
        checkStatement local statement

      -- case lower ... upper: stmt
      -- GNU extension, maybe don't bother to support?
      CCases lower upper statement _ -> do
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
        checkStatement local body

      -- while (expr) stmt / do stmt while (expr);
      -- Treat as if (expr) stmt else ;
      CWhile condition body isDoWhile _ -> do
        checkStatement local body

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
      CGoto label _ -> return local

      -- goto expr;
      CGotoPtr expression _ -> return local

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
      CBlockDecl declaration -> return local  -- TODO: checkLocalDeclaration
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
          CVar ident@(Ident name _ _) _
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
    checkInitializerList = foldlM
      $ \ local (_partDesignators, initializer) -> case initializer of
        CInitExpr expression _ -> checkExpression local expression
        CInitList initializers _ -> checkInitializerList local initializers

    applyPreAction :: LocalContext -> PermissionAction -> IO LocalContext
    applyPreAction local (PermissionAction action permission)
      | action /= Grant = do
      applyPermissionAction NoReason local (PermissionAction Grant permission)
    applyPreAction local _ = return local

    applyPermissionAction :: Reason -> LocalContext -> PermissionAction -> IO LocalContext
    applyPermissionAction reason local (PermissionAction action permission) = do
      case action of
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
            warn $ concat
              [ "ward warning: granting permission '"
              , show permission
              , "' already present in context "
              , show $ Set.toList $ localPermissionState local
              ]
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
              [ "ward warning: revoking permission '"
              , show permission
              , "' not present in context "
              , show $ Set.toList $ localPermissionState local
              ]
            return local

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

noop :: (Monad m) => m ()
noop = return ()

newtype Permission = Permission Text
  deriving (Eq, IsString, Ord)

instance Show Permission where
  show (Permission name) = Text.unpack name

defaultPermissions :: [Permission]
defaultPermissions = ["alloc", "fail", "signal_unsafe"]

defaultPreprocessorFlags :: [String]
defaultPreprocessorFlags = ["-D__WARD__"]

-- | Why a particular permission action is being applied.
data Reason
  = NoReason
  | BecauseCall !Ident

instance Show Reason where
  show = \ case
    NoReason -> "unspecified reason"
    BecauseCall (Ident name _ _) -> concat ["call to '", name, "'"]

-- | An action to take on the context, given the permission from a
-- 'PermissionAction'.
data Action

  -- | The context must contain the given permission. This action does not
  -- change the context.
  = Need

  -- | After this action, the given permission is added to the context. The
  -- context may contain the permission already.
  | Grant

  -- | The context must contain the given permission. After this action, it will
  -- be removed from the context.
  | Revoke
  deriving (Eq, Ord)

instance Show Action where
  show action = case action of
    Need -> "need"
    Grant -> "grant"
    Revoke -> "revoke"

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

-- | A pair of an action and a permission, such as @grant(foo)@.
data PermissionAction = PermissionAction !Action !Permission
  deriving (Eq, Ord)

instance Show PermissionAction where
  show (PermissionAction action permission)
    = concat [show action, "(", show permission, ")"]

globalContextFromTranslationUnit :: CTranslUnit -> GlobalContext
globalContextFromTranslationUnit (CTranslUnit externalDeclarations _)
  = foldr insertTopLevelElement mempty externalDeclarations

data WardException
  = UnknownPermissionActionException String
  deriving (Show, Typeable)

instance Exception WardException

insertTopLevelElement :: CExtDecl -> GlobalContext -> GlobalContext
insertTopLevelElement element global = case element of

  -- For an external declaration, record the permission actions in the context.
  CDeclExt (CDecl specifiers fullDeclarators _) -> global
    { globalPermissionActions = foldr (uncurry (Map.insertWith (<>)))
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
    (CDeclr (Just ident) _ _ _ _) _parameters body _) -> global
      { globalPermissionActions = Map.insertWith (<>)
        ident specifierPermissions $ globalPermissionActions global
      , globalFunctions = Map.insert ident definition
        $ globalFunctions global
      }
    where
      specifierPermissions = extractPermissionActions
        [attr | CTypeQual (CAttrQual attr) <- specifiers]

  CFDefExt{} -> global  -- TODO: warn?
  CAsmExt{} -> global  -- TODO: warn?

extractDeclaratorPermissionActions
  :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> [(Ident, Set PermissionAction)]
extractDeclaratorPermissionActions = foldr go []
  where
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
    _ -> throw $ UnknownPermissionActionException actionName
  return $ PermissionAction action $ Permission $ Text.pack permission
