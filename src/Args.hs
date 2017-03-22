module Args
  ( Args(..)
  , Flag(..)
  , parse
  , usage
  ) where

import Control.Arrow (second)
import Data.Foldable (asum)
import Data.List (partition, stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Args = Args
  { configFilePaths :: [FilePath]
  , implicitPermissions :: [String]
  , preprocessorFlags :: [String]
  , preprocessorPath :: FilePath
  , translationUnitPaths :: [FilePath]
  }

data Flag = GrantFlag String | ConfigFlag FilePath

parse :: IO Args
parse = do

  args <- getArgs

  (ppPath, filePaths, wardFlags, ppFlags) <- case args of
    [] -> usage
    [_] -> usage
    pp : rest -> let
      (wardArgs, ppFlags') = second (drop 1) $ break (== "--") rest
      isFlag ('-' : _) = True
      isFlag _ = False
      (unparsedFlags, paths) = partition isFlag wardArgs
      in return (pp, paths, traverse parseFlag unparsedFlags, ppFlags')

  parsedFlags <- case wardFlags of
    Right parsed -> return parsed
    Left flagError -> do
      hPutStrLn stderr $ concat ["Unknown flag '", flagError, "'"]
      usage

  return Args
    { configFilePaths = [path | ConfigFlag path <- parsedFlags]
    , implicitPermissions = [permission | GrantFlag permission <- parsedFlags]
    , preprocessorFlags = defaultPreprocessorFlags ++ ppFlags
    , preprocessorPath = ppPath
    , translationUnitPaths = filePaths
    }

parseFlag :: String -> Either String Flag
parseFlag arg = maybe (Left arg) Right $ asum $ map try
  [ (GrantFlag, "--grant=")
  , (GrantFlag, "-G")
  , (ConfigFlag, "--config=")
  , (ConfigFlag, "-C")
  ]
  where
    try (f, prefix) = f <$> stripPrefix prefix arg

defaultPreprocessorFlags :: [String]
defaultPreprocessorFlags = ["-D__WARD__"]

usage :: IO a
usage = do
  hPutStrLn stderr "\
\SYNOPSIS\n\
\    ward <cpp> <path>* [--grant=<perm> | -G<perm>] [-- <flags>]\n\
\\n\
\OPTIONS\n\
\    <cpp>\n\
\        Name of preprocessor (gcc)\n\
\\n\
\    <path>\n\
\        Path to C translation unit (foo.c)\n\
\\n\
\    <flags>\n\
\        Flags for C preprocessor (-D HAVE_FOO -I /bar/baz)\n\
\\n\
\    --grant=<perm>\n\
\    -G<perm>\n\
\        Implicitly grant <perm> unless explicitly waived.\n\
\\&"
  exitFailure
