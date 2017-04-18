{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Args
  ( Args(..)
  , parse
  ) where

import Control.Arrow (returnA)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import Options.Applicative
import Options.Applicative.Arrows (asA, runA)
import Types

data Args = Args
  { configFilePaths :: [FilePath]
  , implicitPermissions :: [String]
  , outputMode :: !OutputMode
  , preprocessorFlags :: [String]
  , preprocessorPath :: FilePath
  , quiet :: Bool
  , translationUnitPaths :: [FilePath]
  }

parse :: IO Args
parse = execParser $ info (args <**> helper)
  $ fullDesc
    <> progDesc "Verify Ward permissions for all top-level functions."
    <> header "Ward - A static analysis tool for C."

args :: Parser Args
args = runA $ proc () -> do

  preprocessorPath <- opt strArgument
    [ metavar "CPP"
    , help "Name of preprocessor."
    ] -< ()

  outputMode <- opt
    (fmap (fromMaybe CompilerOutput)
      . optional . option parseOutputMode)
    [ long "mode"
    , short 'M'
    , metavar "html|compiler"
    , help "Output mode style (default 'compiler')."
    ] -< ()

  configFilePaths <- opt (many . strOption)
    [ long "config"
    , short 'C'
    , metavar "FILE"
    , help "Read permission information from configuration FILE."
    ] -< ()

  quiet <- opt (flag False True)
    [ long "quiet"
    , short 'q'
    , help "Suppress output except for errors."
    ] -< ()

  implicitPermissions <- opt (many . strOption)
    [ long "grant"
    , short 'G'
    , metavar "PERM"
    , help "Implicitly grant PERM unless explicitly waived."
    ] -< ()

  translationUnitPaths <- opt (some . strArgument)
    [ metavar "PATH..."
    , help "Paths to C source files."
    ] -< ()

  preprocessorFlags <- opt
    (fmap (defaultPreprocessorFlags <>) . many . strOption)
    [ long "preprocess"
    , short 'P'
    , metavar "FLAG"
    , help "Pass FLAG to preprocessor."
    ] -< ()

  returnA -< Args{..}

  where opt f xs = asA $ f $ mconcat xs

parseOutputMode :: ReadM OutputMode
parseOutputMode = eitherReader $ \ case
  "compiler" -> Right CompilerOutput
  "html" -> Right HtmlOutput
  mode -> Left $ concat
    [ "Unknown output mode '"
    , mode
    , "'. Valid modes are 'compiler' and 'html'."
    ]

defaultPreprocessorFlags :: [String]
defaultPreprocessorFlags = ["-D__WARD__"]
