{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Args (Args(Args))
import Data.IORef -- *
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Test.HUnit
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
        $ \ entries -> do
        assertBool "reports one error" $ case entries of
          [Error _ "unknown permission action 'require'; ignoring"] -> True
          _ -> False

    it "reports multiple invalid permission actions" $ do
      wardTest defArgs
        { Args.translationUnitPaths = ["test/invalid-permission-actions.c"] }
        $ \ entries -> do
        assertBool "reports two errors" $ case entries of
          [ Error _ "unknown permission action 'require'; ignoring"
            , Error _ "unknown permission action 'wave'; ignoring"
            ] -> True
          _ -> False

defArgs :: Args
defArgs = Args
  { Args.preprocessorPath = "gcc"
  , Args.translationUnitPaths = []
  , Args.flags = []
  , Args.preprocessorFlags = []
  }

wardTest :: Args -> ([Entry] -> IO ()) -> IO ()
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
      check entries
