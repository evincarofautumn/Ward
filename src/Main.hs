{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Check.Permissions (Function(..))
import Config
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (unless)
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.System.GCC (newGCC)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)
import Types
import qualified Args
import qualified Check.Permissions as Permissions
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified DumpCallMap
import qualified Graph

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do

  args <- Args.parse

  unless (null $ Args.configFilePaths args) $ do
    logProgress args "Loading config files..."
  config <- do
    parsedConfigs <- traverse Config.fromFile $ Args.configFilePaths args
    case sequence parsedConfigs of
      Right configs -> pure $ mconcat configs
      Left parseError -> do
        hPutStrLn stderr $ "Config parse error:\n" ++ show parseError
        exitFailure
  logProgress args "Preprocessing and parsing..."
  parseResults <- let
    temporaryDirectory = Nothing
    preprocessor = newGCC $ Args.preprocessorPath args
    in forM (Args.translationUnitPaths args)
      $ parseCFile preprocessor temporaryDirectory
      $ Args.preprocessorFlags args
  case sequence parseResults of
    Right translationUnits ->
      case Args.outputAction args of
        AnalysisAction outputMode -> analyze args outputMode config translationUnits
        GraphAction -> dumpCallGraph args config translationUnits
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError

analyze :: Args.Args -> OutputMode -> Config -> [CTranslUnit] -> IO ()
analyze args outputMode config translationUnits = do
  logProgress args "Checking..."
    putStr $ formatHeader outputMode
    do
      entriesChan <- newChan
      _checkThread <- forkIO $ flip runLogger entriesChan $ do
        let
          callMap = Graph.fromTranslationUnits config
            (zip (Args.translationUnitPaths args) translationUnits)
          functions = map
            (\ (name, (pos, calls, permissions)) -> Function
              { functionPos = pos
              , functionName = nameFromIdent name
              , functionPermissions = permissions
              , functionCalls = nameFromIdent <$> calls
              })
            $ Map.toList callMap
            where
              nameFromIdent (Ident name _ _) = Text.pack name
        mapM_ (record True) $ Permissions.validatePermissions config callMap
        Permissions.process functions config
        endLog

      let
        loop !warnings !errors seen = do
          message <- readChan entriesChan
          case message of
            Nothing -> return (warnings, errors)
            Just entry
              | entry `elem` seen -> loop warnings errors seen
              | otherwise -> do
              putStrLn $ format outputMode entry
              let seen' = entry : seen
              case entry of
                Note{} -> loop warnings errors seen'
                Warning{} -> loop (warnings + 1) errors seen'
                Error{} -> loop warnings (errors + 1) seen'

      (warnings, errors) <- loop (0 :: Int) (0 :: Int) []

      putStr $ formatFooter outputMode $ concat
        [ "Warnings: ", show warnings
        , ", Errors: ", show errors
        ]

logProgress :: Args.Args -> String -> IO ()
logProgress args s = case Args.outputAction args of
  AnalysisAction HtmlOutput -> return ()
  AnalysisAction CompilerOutput -> hPutStrLn stdout s
  GraphAction -> hPutStrLn stderr s

dumpCallGraph :: Args.Args -> Config -> [CTranslUnit] -> IO ()
dumpCallGraph args config translationUnits = do
  let callMap = Graph.fromTranslationUnits config
        (zip (Args.translationUnitPaths args) translationUnits)
  DumpCallMap.hPutCallMap stdout callMap

