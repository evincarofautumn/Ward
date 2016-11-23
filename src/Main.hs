{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import System.IO (hPutStrLn, stderr)
import Types
import qualified Args
import qualified Check
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
  let
    implicitPermissions = Set.fromList
      [ Permission $ Text.pack permission
      | Args.GrantFlag permission <- Args.flags args
      ]
  case sequence parseResults of
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError
    Right translationUnits -> do
      Check.translationUnits translationUnits implicitPermissions
