{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Check.Permissions (Function(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (forM)
import Language.C (parseCFile)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.System.GCC (newGCC)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types
import qualified Args
import qualified Check.Permissions as Permissions
import qualified Config
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Graph

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do

  args <- Args.parse

  unless (null $ Args.configFilePaths args) $ do
    when (Args.outputMode args == CompilerOutput) $ do
      putStrLn "Loading config files..."
  _config <- do
    parsedConfigs <- traverse Config.fromFile $ Args.configFilePaths args
    case sequence parsedConfigs of
      Right configs -> pure $ mconcat configs
      Left parseError -> do
        hPutStrLn stderr $ "Config parse error:\n" ++ show parseError
        exitFailure

  when (Args.outputMode args == CompilerOutput) $ do
    putStrLn "Preprocessing..."
  parseResults <- let
    temporaryDirectory = Nothing
    preprocessor = newGCC $ Args.preprocessorPath args
    in forM (Args.translationUnitPaths args)
      $ parseCFile preprocessor temporaryDirectory
      $ Args.preprocessorFlags args

  when (Args.outputMode args == CompilerOutput) $ do
    putStrLn "Checking..."
  case sequence parseResults of
    Right translationUnits -> do

      putStr $ formatHeader $ Args.outputMode args

      entriesChan <- newChan
      _checkThread <- forkIO $ flip runLogger entriesChan $ do
        let
          implicitPermissions = Set.fromList
            $ map (PermissionName . Text.pack)
            $ Args.implicitPermissions args
          callMap = Graph.fromTranslationUnits implicitPermissions
            (zip (Args.translationUnitPaths args) translationUnits)
          functions = map
            (\ (name, (calls, permissions)) -> Function
              { functionName = nameFromIdent name
              , functionPermissions = permissions
              , functionCalls = Vector.fromList
                $ map (Vector.singleton . nameFromIdent) calls
              })
            $ Map.toList callMap
            where
              nameFromIdent (Ident name _ _) = Text.pack name
          restrictions = []
        liftIO $ Permissions.process functions restrictions
        endLog

      let
        loop !warnings !errors = do
          message <- readChan entriesChan
          case message of
            Nothing -> return (warnings, errors)
            Just entry -> do
              putStrLn $ format (Args.outputMode args) entry
              case entry of
                Note{} -> loop warnings errors
                Warning{} -> loop (warnings + 1) errors
                Error{} -> loop warnings (errors + 1)

      (warnings, errors) <- loop (0 :: Int) (0 :: Int)

      putStr $ formatFooter (Args.outputMode args) $ concat
        [ "Warnings: ", show warnings
        , ", Errors: ", show errors
        ]

    Left parseError -> do
      hPutStrLn stderr $ "Parse error:\n" ++ show parseError
