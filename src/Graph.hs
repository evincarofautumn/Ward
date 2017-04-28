{-# LANGUAGE LambdaCase #-}

module Graph
  (
  ) where

import Data.Generics
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Node (NodeInfo)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST -- *
import Language.C.Syntax.Constants -- *
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

type NameMap = Map Ident (Maybe CFunDef)

type CallMap = Map Ident [Ident]

-- | Builds a call graph from a set of translation units.
fromTranslationUnits :: implicitPermissions -> [(FilePath, CTranslUnit)] -> NameMap
fromTranslationUnits _implicitPermissions = nameMapFromTranslationUnit _implicitPermissions . joinTranslationUnits

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
        names =
          [ ident
          -- TODO: Do something with derived declarators?
          | (Just (CDeclr (Just ident) _derived _ _ _), _, _) <- fullDeclarators
          ]
        in Map.fromList $ zip names $ repeat Nothing

      -- For a function definition, record the function body in the context.
      CFDefExt definition@(CFunDef specifiers
        (CDeclr (Just ident) _ _ _ _) _parameters _body _)
          -> Map.singleton ident (Just definition)

      _ -> mempty

----

callMapFromNameMap :: NameMap -> CallMap
callMapFromNameMap = Map.fromList . mapMaybe fromEntry . Map.toList
  where
    fromEntry (name, mDef) = do
      def <- fromFunction <$> mDef
      Just (name, def)

    fromFunction :: CFunDef -> [Ident]
    fromFunction (CFunDef _specifiers
      (CDeclr (Just ident@(Ident name _ pos)) _ _ _ _)
      parameters body _)
      = everything (<>) (mkQ mempty fromStatement) body
      where
        -- TODO: Do something with parameter names?
        parameterNames =
          [ Just parameterName
          | CDecl _ parameterDeclarations _ <- parameters
          , (Just (CDeclr (Just (Ident parameterName _ _)) _ _ _ _), _, _)
            <- parameterDeclarations
          ]
    fromFunction _ = mempty

    fromStatement :: CStat -> [Ident]
    fromStatement = everything (<>) (mkQ mempty fromExpression)
      -- FIXME: Should be if (f()) { g(); } else { h(); } => [[f], [g, h]]
      -- CIf condition true mFalse _
      -- FIXME: Also CSwitch, CWhile, CFor?

    -- This is somewhat approximate, because it doesn't treat conditional
    -- statements or expressions differently from ordinary sequential
    -- calls. Ideally it should collect calls in the order they would be called,
    -- so that the branches of conditionals can be unified rather than checked
    -- in sequence. However, in general this would require building a call
    -- *tree* rather than a call *list* for each function, which is nontrivial.
    --
    -- This also assumes a left-to-right evaluation order for binary expressions
    -- and function arguments, which is standard-compliant but not necessarily
    -- the same as what your compiler does.

    fromExpression :: CExpr -> [Ident]
    fromExpression = \ case
      -- FIXME: Should be f() ? g() : h() => [[f], [g, h]]
      -- CCond a mb c _
      -- FIXME: Should be f(g(), h(), ...) => [[g], [h], [f]]
      CCall (CVar ident _) arguments callPos -> [ident]
      _ -> []

