{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Check.Permissions (Function(..))
import Control.Concurrent.Chan (getChanContents, newChan)
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (NodeInfo, posOfNode)
import Language.C.Data.Position (Position, position, posFile, posRow, posColumn, posOffset)
import Language.C.System.GCC (newGCC)
import Test.HUnit hiding (errors)
import Test.Hspec
import Text.Parsec (ParseError)
import qualified Check.Permissions as Permissions
import qualified Data.Map as Map
import qualified Data.Text as Text

import Types
import Args (Args(Args))
import qualified Args
import qualified Config
import qualified DumpCallMap
import qualified Graph
import qualified ParseCallMap

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "with config files" $ do

    it "accepts empty config" $ do
      configTest "" $ Right mempty

    it "accepts comment at end of file" $ do
      configTest
        "// comment"
        $ Right mempty

    it "accepts multiple comments" $ do
      configTest
        "// comment\n\
        \// comment\n"
        $ Right mempty

    it "accepts permission declaration" $ do
      configTest
        "perm1;"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1" mempty }

    it "accepts permission declaration with modifier" $ do
      configTest
        "perm1 implicit;"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1"
          mempty { declImplicit = True }
        }

    it "accepts permission declaration with description" $ do
      configTest
        "perm1 \"permission the first\";"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1"
          mempty { declDescription = Just "permission the first" }
        }

    it "accepts multiple permission declarations" $ do
      configTest
        "perm1; perm2;"
        $ Right $ mempty
        { configDeclarations = Map.fromList
          [ ("perm1", mempty)
          , ("perm2", mempty)
          ]
        }

    it "accepts relationship declaration" $ do
      configTest
        "perm1 -> perm2;"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1"
          $ Declaration False Nothing [("perm2", Nothing)]
        }

    it "accepts relationship declaration with description" $ do
      configTest
        "perm1 -> perm2 \"perm1 implies perm2\";"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1"
          $ Declaration False Nothing
            [ ("perm2", Just "perm1 implies perm2")
            ]
        }

    it "accepts relationship declaration with complex expression" $ do
      configTest
        "p1 -> p2 & p3 | p4 & !p5 | !(p6 & p7);"
        $ Right $ mempty
        { configDeclarations = Map.singleton "p1"
          $ Declaration False Nothing
            [ ("p2" `And` "p3" `Or` "p4" `And` Not "p5" `Or` Not ("p6" `And` "p7"), Nothing)
            ]
        }

    it "accepts enforcement with path" $ do
      configTest
        ".enforce \"foo.c\";"
        $ Right $ mempty
        { configEnforcements = [EnforcePath "foo.c"] }

    it "accepts enforcement with name" $ do
      configTest
        ".enforce bar;"
        $ Right $ mempty
        { configEnforcements = [EnforceFunction "bar"] }

    it "accepts enforcement with path and name" $ do
      configTest
        ".enforce \"foo.c\" bar;"
        $ Right $ mempty
        { configEnforcements = [EnforcePathFunction "foo.c" "bar"] }

  describe "with simple errors" $ do

{-
    it "reports invalid permission actions" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/invalid-permission-action.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool (unlines $ "expected action error but got:" : map show errors)
          $ case errors of
            [(_, "unknown permission action 'require'; ignoring")] -> True
            _ -> False

    it "reports multiple invalid permission actions" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/invalid-permission-actions.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected action error but got:" : map show errors)
          $ case errors of
            [ (_, "unknown permission action 'wave'; ignoring")
              , (_, "unknown permission action 'require'; ignoring")
              ] -> True
            _ -> False
-}

    it "reports missing permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/missing-permission.c"]
        , Args.configFilePaths = ["test/missing-permission.config"]
        } $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [(_, "missing required annotation on 'bar';\
                 \ annotation [] is missing: [need(baz)]")] -> True
            _ -> False

    it "reports disallowed permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/disallowed-permission.c"]
        } $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            -- with permissions '[lacks(baz),has(baz)]' 
            [(_, "conflicting information for permissions [baz] in 'bar'")] -> True
            _ -> False

    it "does not report missing implicit permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/missing-implicit-permission.c"]
        , Args.configFilePaths = ["test/missing-implicit-permission.config"]
        } $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected no error but got:" : map show errors)
          $ case errors of
            [] -> True
            _ -> False

    it "reports descriptions of violated restrictions" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/violated-restriction.c"]
        , Args.configFilePaths = ["test/violated-restriction.config"]
        } $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [ (_, "restriction \"cannot take foo lock while bar lock is held\" (uses(lock_foo) -> !has(bar_locked)) violated in 'locks_wrong_nesting' at \"lock_bar\"")
              , (_, "conflicting information for permissions [foo_locked,lock_foo] in 'locks_foo_recursively'")
              , (_, "restriction \"cannot take foo lock recursively\" (uses(lock_foo) -> !has(foo_locked)) violated in 'locks_foo_recursively' before first call")
              , (_, "restriction \"cannot take foo lock recursively\" (uses(lock_foo) -> !has(foo_locked)) violated in 'locks_foo_recursively' at \"lock_foo\"")
              , (_, "missing required annotation on 'missing_foo_locked'; annotation [] is missing: [need(foo_locked)]")
              ] -> True
            _ -> False

  describe "with callmap serialization" $ do
    it "roundtrips"  $ do
      let
        withFile fp = defArgs { Args.translationUnitPaths = [fp] }
      traverse_ (callMapRoundtripTest . withFile)
        ["test/disallowed-permission.c"
        , "test/disallowed-with-subject.c"
        , "test/invalid-permission-action.c"
        , "test/invalid-permission-actions.c"
        , "test/missing-implicit-permission.c"
        , "test/missing-permission.c"
        , "test/missing-permissions-conditional.c"
        , "test/permission-with-subject.c"
        , "test/violated-restriction.c"
        ]

defArgs :: Args
defArgs = Args
  { Args.preprocessorPath = "gcc"
  , Args.translationUnitPaths = []
  , Args.outputAction = AnalysisAction CompilerOutput
  , Args.outputFilePath = Nothing
  , Args.preprocessorFlags = []
  , Args.configFilePaths = []
  , Args.quiet = False
  }

configTest :: String -> Either ParseError Config -> IO ()
configTest source expected = Config.fromSource "test" source `shouldBe` expected

wardTest
  :: Args
  -> (([(NodeInfo, Text)], [(NodeInfo, Text)], [(NodeInfo, Text)]) -> IO ())
  -> IO ()
wardTest args check =
  wardTestWithCallmaps args $ \ config callMap -> do
      entriesChan <- newChan
      flip runLogger entriesChan $ do
        let
          functions = map
            (\ (name, (pos, calls, permissions)) -> Function
              { functionPos = pos
              , functionName = nameFromIdent name
              , functionPermissions = permissions
              , functionCalls = nameFromIdent <$> calls
              })
            $ Map.toList $ getCallMap callMap
            where
              nameFromIdent (Ident name _ _) = Text.pack name
        Permissions.process functions config
        endLog
      entries <- nub . map fromJust . takeWhile isJust <$> getChanContents entriesChan
      check (partitionEntries entries)

wardTestWithCallmaps :: Args -> (Config -> CallMap -> IO ()) -> IO ()
wardTestWithCallmaps args check = do
  let temporaryDirectory = Nothing
  let preprocessor = newGCC $ Args.preprocessorPath args
  parseResults <- forM (Args.translationUnitPaths args)
    $ fmap (fmap (either (Left  . CSourceUnitParseError) (Right . CSourceProcessingUnit)))
    $ parseCFile preprocessor temporaryDirectory
    $ Args.preprocessorFlags args
  config <- do
    parsedConfigs <- traverse Config.fromFile $ Args.configFilePaths args
    case sequence parsedConfigs of
      Right configs -> pure $ mconcat configs
      Left parseError -> error $ "bad config in test: " ++ show parseError
  case sequence parseResults of
    Left parseError -> assertFailure $ "Parse error: " ++ show parseError
    Right translationUnits -> do
      let
        callMap = Graph.fromProcessingUnits config
          (zip (Args.translationUnitPaths args) translationUnits)
      check config callMap

callMapRoundtripTest :: Args -> IO ()
callMapRoundtripTest args =
  wardTestWithCallmaps args $ \ _config callMap ->
  let
    bs = DumpCallMap.encodeCallMap callMap
    parseResult = ParseCallMap.fromSource bs
  in
    fmap getCallMap parseResult `shouldBeUptoPos` Right (getCallMap callMap)

  where
    shouldBeUptoPos x y = fmap simplifiedPosInfo x `shouldBe` fmap simplifiedPosInfo y

    -- the full NodeInfo has things we don't care to preserve (node names), and
    -- additionally Position includes the full include stack which we don't
    -- preserve in the graph dump.
    simplifiedPosInfo :: Functor f => f (NodeInfo, b, c) -> f (Position, b, c)
    simplifiedPosInfo = fmap $ \(ni, b, c) -> (simplifyPos (posOfNode ni), b, c)
    simplifyPos p = position (posOffset p) (posFile p) (posRow p) (posColumn p) Nothing
