{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.IORef -- *
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types
import qualified Args
import qualified Check
import qualified Config
import qualified Data.Set as Set
import qualified Data.Text as Text

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do

  args <- Args.parse

  unless (null $ Args.configFilePaths args) $ do
    putStrLn "Loading config files..."
  _config <- do
    parsedConfigs <- traverse Config.fromFile $ Args.configFilePaths args
    case sequence parsedConfigs of
      Right configs -> pure $ mconcat configs
      Left parseError -> do
        hPutStrLn stderr $ "Config parse error:\n" ++ show parseError
        exitFailure

  putStrLn "Preprocessing..."
  parseResults <- let
    temporaryDirectory = Nothing
    preprocessor = newGCC $ Args.preprocessorPath args
    in forM (Args.translationUnitPaths args)
      $ parseCFile preprocessor temporaryDirectory
      $ Args.preprocessorFlags args

  putStrLn "Checking..."
  case sequence parseResults of
    Right translationUnits -> do
      entriesRef <- newIORef []
      flip runLogger entriesRef $ let
        implicitPermissions = Set.fromList
          $ map (Permission . Text.pack)
          $ Args.implicitPermissions args
        in Check.translationUnits
          translationUnits
          implicitPermissions
          (Args.quiet args)
      entries <- readIORef entriesRef
      mapM_ print entries
      let (_notes, warnings, errors) = partitionEntries entries
      putStrLn $ concat
        [ "Warnings: ", show (length warnings)
        , ", Errors: ", show (length errors)
        ]
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError
