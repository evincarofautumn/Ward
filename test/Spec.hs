{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Args (Args(Args))
import Config (Config(..), Declaration(Declaration))
import Control.Concurrent.Chan (getChanContents, newChan)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.Data.Node (NodeInfo)
import Language.C.System.GCC (newGCC)
import Test.HUnit hiding (errors)
import Test.Hspec
import Text.Parsec (ParseError)
import Types
import qualified Args
import qualified Config
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

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
          mempty { Config.declImplicit = True }
        }

    it "accepts permission declaration with description" $ do
      configTest
        "perm1 \"permission the first\";"
        $ Right $ mempty
        { configDeclarations = Map.singleton "perm1"
          mempty { Config.declDescription = Just "permission the first" }
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

  describe "with simple errors" $ do

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

    it "reports missing permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/missing-permission.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [(_, "because of call to 'foo', need permission 'baz' not present in context []")] -> True
            _ -> False

    it "reports disallowed permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/disallowed-permission.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool
          (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [(_, "because of call to 'foo', denying disallowed permission 'baz' present in context [baz]")] -> True
            _ -> False

  describe "with local permissions" $ do

    it "reports missing permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/permission-with-subject.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [ (_, "because of call to 'requires_lock',\
                  \ need permission 'locked' for variable 'q'\
                  \ not present in context []")
              , (_, "because of call to 'requires_lock',\
                    \ need permission 'locked' for variable 'p'\
                    \ not present in context []")
              ]
              -> True
            _ -> False


    it "reports disallowed permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/disallowed-with-subject.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [ (_, "because of call to 'lock',\
                  \ denying disallowed permission 'locked' for variable 'p'\
                  \ present in context [locked]")
              , (_, "because of call to 'denies_lock',\
                    \ denying disallowed permission 'locked' for variable 'p'\
                    \ present in context [locked]")
              , (_, "because of call to 'requires_lock',\
                    \ need permission 'locked' for variable 'p'\
                    \ not present in context []")
              ]
              -> True
            _ -> False

defArgs :: Args
defArgs = Args
  { Args.preprocessorPath = "gcc"
  , Args.translationUnitPaths = []
  , Args.implicitPermissions = []
  , Args.outputMode = CompilerOutput
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
wardTest args check = do
  let temporaryDirectory = Nothing
  let preprocessor = newGCC $ Args.preprocessorPath args
  parseResults <- forM (Args.translationUnitPaths args)
    $ parseCFile preprocessor temporaryDirectory
    $ Args.preprocessorFlags args
  let
    implicitPermissions = Set.fromList
      $ map (PermissionName . Text.pack)
      $ Args.implicitPermissions args
  case sequence parseResults of
    Left parseError -> assertFailure $ "Parse error: " ++ show parseError
    Right translationUnits -> do
      entriesChan <- newChan
      flip runLogger entriesChan $ do
        let quiet = False
        endLog
      entries <- map fromJust . takeWhile isJust <$> getChanContents entriesChan
      check (partitionEntries entries)
