{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Args (Args(Args))
import Data.IORef -- *
import Data.Text (Text)
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.Data.Node (NodeInfo)
import Language.C.System.GCC (newGCC)
import Test.HUnit hiding (errors)
import Test.Hspec
import Types
import qualified Args
import qualified Check
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
            [ (_, "unknown permission action 'require'; ignoring")
              , (_, "unknown permission action 'wave'; ignoring")
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

  describe "with local permissions" $ do

    it "reports missing permission" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/permission-with-subject.c"] }
        $ \ (_notes, _warnings, errors) -> do
        assertBool (unlines $ "expected permission error but got:" : map show errors)
          $ case errors of
            [ (_, "because of call to 'requires_lock',\
                  \ need permission 'locked' for variable 'p'\
                  \ not present in context []")
              , (_, "because of call to 'requires_lock',\
                    \ need permission 'locked' for variable 'q'\
                    \ not present in context []")]
              -> True
            _ -> False

defArgs :: Args
defArgs = Args
  { Args.preprocessorPath = "gcc"
  , Args.translationUnitPaths = []
  , Args.flags = []
  , Args.preprocessorFlags = []
  }

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
      [ Permission $ Text.pack permission
      | Args.GrantFlag permission <- Args.flags args
      ]
  case sequence parseResults of
    Left parseError -> assertFailure $ "Parse error: " ++ show parseError
    Right translationUnits -> do
      entriesRef <- newIORef []
      flip runLogger entriesRef
        $ Check.translationUnits translationUnits implicitPermissions
      entries <- readIORef entriesRef
      check (partitionEntries entries)
