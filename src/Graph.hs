{-# LANGUAGE LambdaCase #-}

module Graph
  ( fromTranslationUnits
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.List (ListT(ListT), runListT)
import Data.Foldable (foldrM)
import Data.Functor.Identity (runIdentity)
import Data.Generics
import Data.Monoid ((<>))
import Language.C.Data.Ident (Ident(..))
import Language.C.Syntax.AST -- *
import Types
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Builds a call graph from a set of translation units.
fromTranslationUnits :: implicitPermissions -> [(FilePath, CTranslUnit)] -> CallMap
fromTranslationUnits _implicitPermissions
  = callMapFromNameMap
  . nameMapFromTranslationUnit _implicitPermissions
  . joinTranslationUnits

-- | Joins multiple translation units into one.
joinTranslationUnits :: [(FilePath, CTranslUnit)] -> CTranslUnit
joinTranslationUnits tus@((_, CTranslUnit _ firstLocation) : _)
  = CTranslUnit
    (concat
      [ prefixStatics path externalDeclarations
      | (path, CTranslUnit externalDeclarations _) <- tus
      ])
    firstLocation
joinTranslationUnits [] = error "joinTranslationUnits: empty input"

-- | Prefixes static function names with the name of the translation unit where
-- they were defined.
prefixStatics :: FilePath -> [CExtDecl] -> [CExtDecl]
prefixStatics path decls = map prefixOne decls
  where
    statics =
      [ name
      | CFDefExt (CFunDef specifiers (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _) <- decls
      , CStatic _ : _ <- [[spec | CStorageSpec spec <- specifiers]]
      ]
    prefixOne decl = case decl of
      CFDefExt (CFunDef specifiers
        declr@(CDeclr (Just (Ident name hash namePos)) derived lit attrs declrPos) parameters body defPos)
        | name `elem` statics
        -> CFDefExt (CFunDef specifiers (CDeclr (Just ident') derived lit attrs declrPos) parameters body' defPos)
        | otherwise
        -> CFDefExt (CFunDef specifiers declr parameters body' defPos)
        where
          -- FIXME: Dunno if keeping the same hash is correct.
          ident' = Ident (patchName name) hash namePos
          body' = everywhere (mkT patchStatement) body
      _ -> decl

    patchName :: String -> String
    patchName name = concat [path, "`", name]

    patchStatement :: CStat -> CStat
    patchStatement = everywhere (mkT patchExpression)

    patchExpression :: CExpr -> CExpr
    patchExpression = \ case
      CCall (CVar (Ident name hash namePos) varPos) arguments callPos
        | name `elem` statics
        -> CCall (CVar (Ident (patchName name) hash namePos) varPos)
          arguments callPos
      expr -> expr

----

nameMapFromTranslationUnit
  :: implicitPermissions -> CTranslUnit -> NameMap
nameMapFromTranslationUnit _implicitPermissions
  (CTranslUnit externalDeclarations _)
  = everything (<>) (mkQ mempty fromDecl) externalDeclarations
  where
    fromDecl = \ case
      -- For an external declaration, record an empty body.
      CDeclExt (CDecl specifiers fullDeclarators _) -> let
        specifierPermissions = extractPermissionActions
          [attr | CTypeQual (CAttrQual attr) <- specifiers]
        -- TODO: Do something with derived declarators?
        declaratorPermissions = extractDeclaratorPermissionActions fullDeclarators
        identPermissions = declaratorPermissions ++
          [ (ident, specifierPermissions)
          | ident <- map fst declaratorPermissions
          ]
        in Map.map ((,) Nothing) $ Map.fromListWith (<>) identPermissions

      -- For a function definition, record the function body in the context.
      CFDefExt definition@(CFunDef specifiers
        (CDeclr (Just ident) _ _ _ _) _parameters _body _) -> let
          specifierPermissions = extractPermissionActions
            [attr | CTypeQual (CAttrQual attr) <- specifiers]
          in Map.singleton ident (Just definition, specifierPermissions)

      _ -> mempty

----

simplify :: CallTree a -> CallTree a
simplify (Sequence a b) = case (simplify a, simplify b) of
  (a', Nop) -> a'
  (Nop, b') -> b'
  (a', b') -> Sequence a' b'
simplify (Choice a b) = case (simplify a, simplify b) of
  (a', Nop) -> a'
  (Nop, b') -> b'
  (a', b') -> Choice a' b'
simplify leaf = leaf

callMapFromNameMap :: NameMap -> CallMap
callMapFromNameMap = Map.fromList . map fromEntry . Map.toList
  where
    fromEntry (name, (mDef, permissions)) = let
      calls = maybe Nop fromFunction mDef
      in (name, (simplify calls, permissions))

    fromFunction :: CFunDef -> CallTree Ident
    fromFunction (CFunDef specifiers
      (CDeclr (Just ident@(Ident name _ pos)) _ _ _ _)
      parameters body _)
      = everything Sequence (mkQ Nop fromStatement) body
      where
        -- TODO: Do something with parameter names?
        parameterNames =
          [ Just parameterName
          | CDecl _ parameterDeclarations _ <- parameters
          , (Just (CDeclr (Just (Ident parameterName _ _)) _ _ _ _), _, _)
            <- parameterDeclarations
          ]

    fromFunction _ = Nop

    fromStatement :: CStat -> CallTree Ident
    fromStatement = \ case
      -- FIXME: Also CSwitch, CWhile, CFor?
      CIf condition true mFalse _ -> Sequence
        (everything Sequence (mkQ Nop fromExpression) condition)
        (Choice
          (everything Sequence (mkQ Nop fromExpression) true)
          (everything Sequence (mkQ Nop fromExpression) mFalse))
      statement -> everything Sequence (mkQ Nop fromExpression) statement

    -- from = everything Sequence (mkQ mempty fromExpression)

    -- This assumes a left-to-right evaluation order for binary expressions and
    -- function arguments, which is standard-compliant but not necessarily the
    -- same as what your compiler does.

    fromExpression :: CExpr -> CallTree Ident
    fromExpression = \ case
      -- FIXME: Should be f() ? g() : h() => [[f], [g, h]]
      CCond a mb c _ -> Sequence
        (everything Sequence (mkQ Nop fromExpression) a)
        (Choice
          (everything Sequence (mkQ Nop fromExpression) mb)
          (everything Sequence (mkQ Nop fromExpression) c))
      -- FIXME: Should be f(g(), h(), ...) => [[g], [h], [f]]
      CCall (CVar ident _) arguments callPos -> Sequence
        (everything Sequence (mkQ Nop fromExpression) arguments)
        (Call ident)
      _ -> Nop

extractPermissionActions :: [CAttr] -> PermissionActionSet
extractPermissionActions attributes = runIdentity . fmap HashSet.fromList . runListT $ do
  CAttr (Ident "permission" _ _) expressions _ <- select attributes
  CCall (CVar (Ident actionName _ _) _) permissions pos <- select expressions
  permissionSpec <- select permissions
  (permission, mSubject) <- case permissionSpec of
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
    other -> mzero
  action <- case actionName of
    "needs" -> return Needs
    -- "deny" -> return Deny
    "grants" -> return Grants
    "revokes" -> return Revokes
    -- "waive" -> return Waive
    -- FIXME: Report unknown permission action.
    _ -> mzero
  -- FIXME: Use mSubject
  return $ action $ PermissionName $ Text.pack permission

extractDeclaratorPermissionActions
  :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)]
  -> [(Ident, PermissionActionSet)]
extractDeclaratorPermissionActions = runIdentity . foldrM go []
  where
    -- TODO: Do something with derived declarators?
    go (Just (CDeclr (Just ident) _derived _ attributes _), _, _) acc = do
      let permissionActions = extractPermissionActions attributes
      return $ if HashSet.null permissionActions
        then acc
        else (ident, permissionActions) : acc
    go _ acc = return acc

select :: (Monad m) => [a] -> ListT m a
select = ListT . return
