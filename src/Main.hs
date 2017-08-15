{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Check.Permissions (Function(..))
import Config
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor(..))
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
import qualified Language.C.Parser as CParser
import qualified ParseCallMap
import qualified System.FilePath as FP
import qualified System.IO as IO

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
  parseResults <- traverse (parseInput args) (Args.translationUnitPaths args)
  case sequence parseResults of
    Right translationUnits ->
      case Args.outputAction args of
        AnalysisAction outputMode -> analyze args outputMode config translationUnits
        GraphAction -> dumpCallGraph args config translationUnits
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError

analyze :: Args.Args -> OutputMode -> Config -> [ProcessingUnit] -> IO ()
analyze args outputMode config translationUnits = do
  logProgress args "Checking..."
  do
    putStr $ formatHeader outputMode
    do
      entriesChan <- newChan
      _checkThread <- forkIO $ flip runLogger entriesChan $ do
        let
          callMap = Graph.fromProcessingUnits config
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

dumpCallGraph :: Args.Args -> Config -> [ProcessingUnit] -> IO ()
dumpCallGraph args config translationUnits = do
  let callMap = Graph.fromProcessingUnits config
        (zip (Args.translationUnitPaths args) translationUnits)
  case Args.outputFilePath args of
    Nothing -> DumpCallMap.hPutCallMap stdout callMap
    Just path -> do
      handle <- IO.openFile path IO.WriteMode
      -- Binary mode and block buffering (here using the default block size) are
      -- recommended in the ByteString documentation for hPutBuilder.
      IO.hSetBinaryMode handle True
      IO.hSetBuffering handle $ IO.BlockBuffering Nothing
      DumpCallMap.hPutCallMap handle callMap
      IO.hClose handle

parseInput :: Args.Args -> FilePath -> IO (Either ProcessingUnitParseError ProcessingUnit)
parseInput args path =
  case classifyPath path of
    CSourcePathClass ->
      (bimap CSourceUnitParseError CSourceProcessingUnit) <$> parseCInput args path
    CallMapPathClass ->
      (bimap CallMapUnitParseError CallMapProcessingUnit) <$> parseGraphInput args path

data PathClass = CSourcePathClass | CallMapPathClass

classifyPath :: FilePath -> PathClass
classifyPath path =
  if FP.takeExtension path == ".graph"
  then CallMapPathClass
  else CSourcePathClass

parseCInput :: Args.Args -> FilePath -> IO (Either CParser.ParseError CTranslUnit)
parseCInput args path =
  let temporaryDirectory = Nothing
      preprocessor = newGCC $ Args.preprocessorPath args
  in parseCFile preprocessor temporaryDirectory (Args.preprocessorFlags args) path

parseGraphInput :: Args.Args -> FilePath -> IO (Either CallMapParseError CallMap)
parseGraphInput _args path =
  ParseCallMap.fromFile path

