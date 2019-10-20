{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.Trans.Except
import Control.Monad (unless)
import Control.Monad.IO.Class
import System.Exit (exitFailure, ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr, stdout)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Language.C.Parser as CParser
import qualified System.FilePath as FP
import qualified System.IO as IO
import Control.Concurrent.Async

import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.System.GCC (newGCC)

import Check.Permissions (Function(..))
import Config
import InternIdents
import qualified Args
import qualified Check.Permissions as Permissions
import qualified ParseCallMap
import qualified DumpCallMap
import qualified Graph
import Types

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

  exitResult <- main' args config
  exitWith exitResult

main' :: Args.Args -> Config -> IO ExitCode
main' args config = do
  logProgress args "Preprocessing and parsing..."
  parseResults <- mapConcurrently (runExceptT . parseInput args) (Args.translationUnitPaths args)
  case sequence parseResults of
    Right callMaps ->
      let callMap = mconcat callMaps
      in case Args.outputAction args of
        AnalysisAction outputMode -> analyze args outputMode config callMap
        GraphAction -> ExitSuccess <$ dumpCallGraph args callMap
    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError
      return (ExitFailure 1)

analyze :: Args.Args -> OutputMode -> Config -> CallMap -> IO ExitCode
analyze args outputMode config callMap = do
  logProgress args "Checking..."
  withOutputFn (Args.outputFilePath args) $ \output -> do
    output $ formatHeader outputMode
    do
      entriesChan <- newChan
      checkThread <- async $ flip runLogger entriesChan $ do
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
        mapM_ (record True) $ Permissions.validatePermissions config callMap
        Permissions.process functions config
        endLog

      link checkThread

      let
        loop !warnings !errors seen = do
          message <- readChan entriesChan
          case message of
            Nothing -> return (warnings, errors)
            Just entry
              | entry `elem` seen -> loop warnings errors seen
              | otherwise -> do
              output $ format outputMode entry
              let seen' = entry : seen
              case entry of
                Note{} -> loop warnings errors seen'
                Warning{} -> loop (warnings + 1) errors seen'
                Error{} -> loop warnings (errors + 1) seen'

      (warnings, errors) <- loop (0 :: Int) (0 :: Int) []

      output $ formatFooter outputMode $ concat
        [ "Warnings: ", show warnings
        , ", Errors: ", show errors
        ]
      let
        exitResult =
          if errors == 0 then ExitSuccess else ExitFailure 1
      return exitResult
  where
    withOutputFn :: Maybe FilePath -> ((String -> IO ()) -> IO r) -> IO r
    withOutputFn Nothing k = k putStr
    withOutputFn (Just outputFilePath) k =
      IO.withFile outputFilePath IO.WriteMode (k . IO.hPutStr)


logProgress :: Args.Args -> String -> IO ()
logProgress args s = case Args.outputAction args of
  AnalysisAction HtmlOutput -> return ()
  AnalysisAction CompilerOutput -> hPutStrLn stdout s
  GraphAction -> hPutStrLn stderr s

dumpCallGraph :: Args.Args -> CallMap -> IO ()
dumpCallGraph args callMap = do
  case Args.outputFilePath args of
    Nothing -> DumpCallMap.hPutCallMap stdout callMap
    Just path -> IO.withFile path IO.WriteMode $ \hdl ->
      DumpCallMap.hPutCallMap hdl callMap

parseInput :: Args.Args -> FilePath -> ExceptT ProcessingUnitParseError IO CallMap
parseInput args path = do
  cm <- case classifyPath path of
    CSourcePathClass -> do
      tu <- either (throwE . CSourceUnitParseError) pure =<< liftIO (parseCInput args path)
      pure $! Graph.fromTranslationUnit tu
    CallMapPathClass ->
      either (throwE . CallMapUnitParseError) pure =<< liftIO (parseGraphInput args path)

  pure $! runInternM (internCallMap cm)

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

parseGraphInput :: Args.Args -> FilePath -> IO (Either String CallMap)
parseGraphInput _args path =
  ParseCallMap.fromFile path

