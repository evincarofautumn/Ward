module Args
  ( Args(..)
  , Flag(..)
  , parse
  , usage
  ) where

import Control.Applicative (Alternative(..))
import Control.Arrow (second)
import Data.List (partition, stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Args = Args
  { preprocessorPath :: FilePath
  , translationUnitPaths :: [FilePath]
  , implicitPermissions :: [String]
  , preprocessorFlags :: [String]
  , configFilePaths :: [FilePath]
  }

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
    Left flagError -> do
      hPutStrLn stderr $ concat ["Unknown flag '", flagError, "'"]
      usage
    Right parsed -> return parsed
  return Args
    { preprocessorPath = ppPath
    , translationUnitPaths = filePaths
    , implicitPermissions = [permission | GrantFlag permission <- parsedFlags]
    , preprocessorFlags = defaultPreprocessorFlags ++ ppFlags
    , configFilePaths = [path | ConfigFlag path <- parsedFlags]
    }

defaultPreprocessorFlags :: [String]
defaultPreprocessorFlags = ["-D__WARD__"]

data Flag = GrantFlag String | ConfigFlag FilePath

parseFlag :: String -> Either String Flag
parseFlag arg = maybe (Left arg) Right $ asum
  [ try GrantFlag "--grant="
  , try GrantFlag "-G"
  , try ConfigFlag "--config="
  , try ConfigFlag "-C"
  ]
  where
    try f prefix = f <$> stripPrefix prefix arg

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
