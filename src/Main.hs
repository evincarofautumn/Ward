module Main where

import Language.C (parseCFile)
import Language.C.Pretty (pretty)
import Language.C.System.GCC (newGCC)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint (render)

main :: IO ()
main = do
  let temporaryDirectory = Nothing
  let preprocessor = newGCC "gcc"
  args <- getArgs
  (filePath, preprocessorFlags) <- case args of
    path : flags -> return (path, flags)
    [] -> do
      hPutStrLn stderr "Usage: ward <path> <flags>"
      exitFailure
  result <- parseCFile preprocessor temporaryDirectory preprocessorFlags filePath
  case result of
    Left parseError -> do
      putStrLn "Parse error:"
      print parseError
    Right translationUnit -> do
      putStrLn "Translation unit:"
      putStrLn $ render $ pretty translationUnit
