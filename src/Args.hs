{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Args
  ( Args(..)
  , parse
  ) where

import Data.Monoid ((<>), mconcat)
import Control.Arrow (returnA)
import Options.Applicative
import Options.Applicative.Arrows (asA, runA)

data Args = Args
  { configFilePaths :: [FilePath]
  , implicitPermissions :: [String]
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
  preprocessorPath <- asA $ argument str $ mconcat
    [ metavar "CPP"
    , help "Name of preprocessor."
    ] -< ()
  configFilePaths <- asA $ many $ strOption $ mconcat
    [ long "config"
    , short 'C'
    , metavar "FILE"
    , help "Read permission information from configuration FILE."
    ] -< ()
  quiet <- asA $ flag False True $ mconcat
    [ long "quiet"
    , short 'q'
    , help "Suppress output except for errors."
    ] -< ()
  implicitPermissions <- asA $ many $ strOption $ mconcat
    [ long "grant"
    , short 'G'
    , metavar "PERM"
    , help "Implicitly grant PERM unless explicitly waived."
    ] -< ()
  translationUnitPaths <- asA $ some $ argument str $ mconcat
    [ metavar "PATH..."
    , help "Paths to C source files."
    ] -< ()
  preprocessorFlags <- asA $ fmap (defaultPreprocessorFlags <>)
    $ many $ strOption $ mconcat
    [ long "preprocess"
    , short 'P'
    , metavar "FLAG"
    , help "Pass FLAG to preprocessor."
    ] -< ()
  returnA -< Args{..}

defaultPreprocessorFlags :: [String]
defaultPreprocessorFlags = ["-D__WARD__"]
