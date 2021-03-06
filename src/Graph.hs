{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module Graph
  ( fromTranslationUnit
  , pattern WardKeyword
  ) where

import Control.DeepSeq (deepseq)
import Control.Monad (mzero)
import Control.Monad.Trans.List (ListT(ListT), runListT)
import Data.Foldable (foldrM)
import Data.Functor.Identity (runIdentity)
import Data.Generics
import Data.List (foldl')
import Data.Monoid ((<>))
import Language.C.Data.Ident (Ident(..))
import qualified Language.C.Data.Ident
import Language.C.Syntax.AST -- \*
import Types
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

-- | An 'Ident' pattern that matches the keyword that introduces Ward annotation attributes.
-- This is a bidirectional pattern - it will match all occurrences of the keyword, and can be used to construct
-- the keyword with an internal position (c.f. 'Language.C.Data.Ident.internalIdent')
pattern WardKeyword :: Ident
pattern WardKeyword <- Ident WardKeywordStr _hash _ni
  where
    WardKeyword = Language.C.Data.Ident.internalIdent WardKeywordStr

-- | A String pattern that matches the keyword that introduces Ward annotation attributes.
pattern WardKeywordStr :: String
pattern WardKeywordStr = "ward"

-- | Builds a call graph from a set of C translation units.
fromTranslationUnit :: FilePath -> CTranslUnit -> CallMap
fromTranslationUnit path
  = callMapFromNameMap
  . nameMapFromTranslationUnit
  . prefixStatics path

-- | Prefixes static function names with the name of the translation unit where
-- they were defined.
prefixStatics :: FilePath -> CTranslUnit -> CTranslUnit
prefixStatics path (CTranslUnit decls x) =
    CTranslUnit (statics `deepseq` map prefixOne decls) x
  where
    statics :: HashSet.HashSet String
    statics =
      HashSet.fromList
      [ name
      | d <- decls
      , name <- namesOfStaticDeclOrDefn d
      ]
    isStatic :: String -> Bool
    isStatic a = HashSet.member a statics

    -- Patch the name of a function definition or declaration that's been identified as static.
    -- In function definitions, also patch all called function names in the function body.
    prefixOne :: CExtDecl -> CExtDecl
    prefixOne decl = case decl of
      CFDefExt (CFunDef specifiers
        declr@(CDeclr (Just ident@(Ident name _ _)) derived lit attrs declrPos) parameters body defPos)
        | isStatic name
        -> body' `seq` CFDefExt (CFunDef specifiers (CDeclr (Just ident') derived lit attrs declrPos) parameters body' defPos)
        | otherwise
        -> body' `seq` CFDefExt (CFunDef specifiers declr parameters body' defPos)
        where
          ident' = patchIdent ident
          body' = patchStatement body
      CDeclExt (CDecl specifiers declarators declPos)
        -> CDeclExt (CDecl specifiers (map patchDeclarator declarators) declPos)
      _ -> decl

    patchName :: String -> String
    patchName name = concat [path, "`", name]

    patchIdent :: Ident -> Ident
    patchIdent (Ident name hash namePos) =
      Ident (patchName name) hash namePos

    patchStatement :: CStat -> CStat
    patchStatement = everywhere (mkT patchExpression)

    patchExpression :: CExpr -> CExpr
    patchExpression = \ case
      CCall (CVar (Ident name hash namePos) varPos) arguments callPos
        | isStatic name
        -> CCall (CVar (Ident (patchName name) hash namePos) varPos)
          arguments callPos
      expr -> expr

    patchDeclarator:: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
    patchDeclarator t = case t of
      (Just (CDeclr (Just ident@(Ident name _ _)) ds@(CFunDeclr {} : _) lit attrs posDeclr), initlz, sz)
        | isStatic name
        -> (Just (CDeclr (Just ident') ds lit attrs posDeclr), initlz, sz)
          where
            ident' = patchIdent ident
      _ -> t

namesOfStaticDeclOrDefn :: CExtDecl -> [String]
namesOfStaticDeclOrDefn =
  \ case
    CFDefExt (CFunDef specifiers (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _)
      | hasStaticSpecifiers specifiers -> [name]
    CDeclExt (CDecl specifiers declarators _)
      | hasStaticSpecifiers specifiers -> concatMap (\(d,_,_) -> namesOfFunDeclarator d) declarators
    _ -> []

namesOfFunDeclarator :: Maybe (CDeclarator a) -> [String]
namesOfFunDeclarator (Just (CDeclr (Just (Ident name _ _)) (CFunDeclr {} : _) _ _ _)) = [name]
namesOfFunDeclarator _ = []

hasStaticSpecifiers :: [CDeclarationSpecifier a] -> Bool
hasStaticSpecifiers specifiers =
  let (storageSpecs, _, _, _, _, _) = partitionDeclSpecs specifiers
  in any isStaticSpec storageSpecs
  where
    isStaticSpec :: CStorageSpecifier a -> Bool
    isStaticSpec (CStatic {}) = True
    isStaticSpec _ = False


----

nameMapFromTranslationUnit
  :: CTranslUnit -> NameMap
nameMapFromTranslationUnit
  (CTranslUnit externalDeclarations _)
  = foldl' combine mempty $ map fromDecl externalDeclarations
  where

    -- Combine name maps, preferring to preserve *declaration* position (if any)
    -- and *definition* body (if any). This allows us to later enforce the
    -- presence of annotations based on file name, e.g., that anything declared
    -- in the public header of a "module" must be annotated.
    combine :: NameMap -> NameMap -> NameMap
    combine = Map.unionWith combine'
      where
        combine' (pos1, mDef1, perm1) (pos2, mDef2, perm2) = case (mDef1, mDef2) of
          (Nothing, Just{}) -> (pos1, mDef2, perms)
          (Just{}, Nothing) -> (pos2, mDef1, perms)
          _ -> (pos1, mDef1, perms)
          where
            perms = perm1 <> perm2

    fromDecl :: CExtDecl -> NameMap
    fromDecl = \ case
      -- For an external declaration, record an empty body.
      CDeclExt (CDecl specifiers fullDeclarators pos) -> let
        specifierPermissions = extractPermissionActions
          [attr | CTypeQual (CAttrQual attr) <- specifiers]
        -- TODO: Do something with derived declarators?
        declaratorPermissions = extractDeclaratorPermissionActions fullDeclarators
        identPermissions = declaratorPermissions ++
          [ (ident, specifierPermissions)
          | ident <- map fst declaratorPermissions
          ]
        in Map.map (\ x -> (pos, Nothing, x))
          $ Map.fromListWith (<>) identPermissions

      -- For a function definition, record the function body in the context.
      CFDefExt definition@(CFunDef specifiers
        (CDeclr (Just ident) _ _ _ _) _parameters body pos) -> let
          specifierPermissions = extractPermissionActions
            [attr | CTypeQual (CAttrQual attr) <- specifiers]
          in body `seq` Map.singleton ident (pos, Just definition, specifierPermissions)

      _ -> mempty

----

-- | Converts a 'NameMap' into a 'CallMap' by converting function bodies into
-- 'CallTree's.
callMapFromNameMap :: NameMap -> CallMap
callMapFromNameMap = CallMap . Map.mapWithKey fromEntry
  where
    fromEntry name (pos, mDef, permissions) = let
      calls = maybe mempty fromFunction mDef
      in (pos, calls, permissions)

    fromFunction :: CFunDef -> CallSequence Ident
    fromFunction (CFunDef _specifiers
      (CDeclr (Just (Ident _name _ _pos)) _ _ _ _)
      parameters body _)
      = fromStatement body
      where
        -- TODO: Do something with parameter names?
        _parameterNames =
          [ Just parameterName
          | CDecl _ parameterDeclarations _ <- parameters
          , (Just (CDeclr (Just (Ident parameterName _ _)) _ _ _ _), _, _)
            <- parameterDeclarations
          ]

    fromFunction _ = mempty

    fromStatement :: CStat -> CallSequence Ident
    fromStatement = \ case
      CLabel _label stat _attrs _pos
        -> fromStatement stat
      CCase expr stat _pos
        -> fromExpression expr
        <> fromStatement stat
      CCases expr1 expr2 stat _pos
        -> fromExpression expr1
        <> fromExpression expr2
        <> fromStatement stat
      CDefault stat _pos
        -> fromStatement stat
      CExpr mExpr _pos
        -> foldMap fromExpression mExpr
      CCompound _localLabels blockItems _pos
        -> foldMap fromBlockItem blockItems
      CIf expr stat1 mStat2 _pos
        -> fromExpression expr
        <> singletonCallSequence (fromStatement stat1 `Choice` foldMap fromStatement mStat2)
      -- switch statement @CSwitch selectorExpr switchStmt@, where
      -- @switchStmt@ usually includes /case/, /break/ and /default/
      -- statements
      CSwitch expr stat _pos
        -> fromExpression expr
        <> fromStatement stat
      CWhile expr stat isDoWhile _pos
        | isDoWhile -> fromStatement stat <> fromExpression expr
        | otherwise -> fromExpression expr <> fromStatement stat
      CFor mExpr1OrDecl mExpr2 mExpr3 stat _pos
        -> either (foldMap fromExpression) fromDeclaration mExpr1OrDecl
        <> foldMap fromExpression mExpr2
        <> foldMap fromExpression mExpr3
        <> fromStatement stat

      -- TODO: Do something more clever with flow control statements?
      CGoto _label _pos
        -> mempty
      CGotoPtr expr _pos
        -> fromExpression expr
      CCont _pos
        -> mempty
      CBreak _pos
        -> mempty
      CReturn mExpr a
        -> foldMap fromExpression mExpr

      -- TODO: Handle effects for assembly statements?
      CAsm _asmStat _pos
        -> mempty

    -- This assumes a left-to-right evaluation order for binary expressions and
    -- function arguments, which is standard-compliant but not necessarily the
    -- same as what your compiler does.

    fromExpression :: CExpr -> CallSequence Ident
    fromExpression = \ case
      CComma exprs _pos
        -> foldMap fromExpression exprs
      CAssign _op expr1 expr2 _pos
        -> fromExpression expr1
        <> fromExpression expr2
      CCond expr1 mExpr2 expr3 _pos
        -> fromExpression expr1
        <> singletonCallSequence (foldMap fromExpression mExpr2 `Choice` fromExpression expr3)
      CBinary _op expr1 expr2 _pos
        -> fromExpression expr1
        <> fromExpression expr2
      -- I'm pretty sure nothing needs to be done with the declaration here.
      CCast _decl expr _pos
        -> fromExpression expr
      CUnary _op expr _pos
        -> fromExpression expr
      CSizeofExpr expr _pos
        -> fromExpression expr
      CSizeofType _decl _pos
        -> mempty
      CAlignofExpr expr _pos
        -> fromExpression expr
      CAlignofType _decl _pos
        -> mempty
      CComplexReal expr _pos
        -> fromExpression expr
      CComplexImag expr _pos
        -> fromExpression expr
      CIndex expr1 expr2 _pos
        -> fromExpression expr1
        <> fromExpression expr2
      CCall (CVar name _) args _pos
        -> foldMap fromExpression args
        <> singletonCallSequence (Call name)
      CCall expr args _pos
        -> foldMap fromExpression args
        <> fromExpression expr
      CMember expr _name _deref _pos
        -> fromExpression expr
      CVar _name _pos
        -> mempty
      CConst _const
        -> mempty
      CCompoundLit _decl initList _pos
        -> fromInitList initList
      -- TODO: This should probably be a choice of the possible cases.
      CGenericSelection expr _cases _pos
        -> mempty
      CStatExpr stat _pos
        -> fromStatement stat
      CLabAddrExpr _label _pos
        -> mempty
      -- TODO: Handle permissions for builtins.
      CBuiltinExpr _builtin
        -> mempty

    fromBlockItem :: CBlockItem -> CallSequence Ident
    fromBlockItem = \ case
      CBlockStmt stat -> fromStatement stat
      CBlockDecl decl -> fromDeclaration decl
      -- TODO: Handle nested functions?
      CNestedFunDef _def -> mempty

    fromDeclaration :: CDecl -> CallSequence Ident
    fromDeclaration (CDecl _specs declarators _pos)
      = mconcat
      [ fromInitializer initializer
      | (_, Just initializer, _) <- declarators
      ]
    fromDeclaration CStaticAssert{}
      = mempty

    fromInitializer :: CInit -> CallSequence Ident
    fromInitializer (CInitExpr expr _pos) = fromExpression expr
    fromInitializer (CInitList initList _pos) = fromInitList initList

    fromInitList :: CInitList -> CallSequence Ident
    fromInitList initList
      = mconcat
      [ fromInitializer initializer
      | (_, initializer) <- initList
      ]

extractPermissionActions :: [CAttr] -> PermissionActionSet
extractPermissionActions attributes = runIdentity . fmap HashSet.fromList . runListT $ do
  CAttr WardKeyword expressions _ <- select attributes
  CCall (CVar (Ident actionName _ _) _) permissions _pos <- select expressions
  permissionSpec <- select permissions
  (permission, _mSubject) <- case permissionSpec of
    CVar (Ident permission _ _) _ -> return (permission, Nothing)
    {-
    -- FIXME: Allow subjects.
    CCall
      (CVar (Ident permission _ _) _)
      [CConst (CIntConst (CInteger subject _ _) _)]
      _
      -> return (permission, Just (fromInteger subject))
    -}
    -- FIXME: Report malformed permission specifier.
    _other -> mzero
  action <- case actionName of
    "need" -> select [Need]
    "use" -> select [Need, Use]
    "deny" -> select [Deny]
    "grant" -> select [Grant]
    "revoke" -> select [Revoke]
    "waive" -> select [Waive]
    -- FIXME: Report unknown permission action.
    _ -> mzero
  -- FIXME: Use mSubject
  return $ action $ PermissionName $ Text.pack permission

extractDeclaratorPermissionActions
  :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)]
  -> [(Ident, PermissionActionSet)]
extractDeclaratorPermissionActions = runIdentity . foldrM go []
  where
    go (Just (CDeclr (Just ident) derivedDeclarators _ attributes _), _, _) acc = do
      let
        permissionActions = extractPermissionActions attributes
        -- We only extract permission actions for function declarators;
        -- otherwise, extern variables would be included, and we don't currently
        -- have a defined semantics for permissions attached to variables.
        functionPermissionActions =
          [ extractPermissionActions funAttrs
          | CFunDeclr _parameters funAttrs _ <- derivedDeclarators
          ]
      pure $ if null functionPermissionActions then acc
        else (ident, mconcat functionPermissionActions <> permissionActions) : acc
    go _ acc = return acc

select :: (Monad m) => [a] -> ListT m a
select = ListT . return
