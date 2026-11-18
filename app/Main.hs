module Main (main) where
import System.Environment (getArgs, getProgName)
import Data.List (isSuffixOf)
import Compiler (compile)
import Finalizer (finalize)

version :: String
version = "n/a"

showUsage :: IO ()
showUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <sourcefile.mcmd>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-v"]  -> putStrLn version
        [path]  ->  if ".mcmd" `isSuffixOf` path then do
                        chains <- compile path
                        putStrLn $ finalize chains
                    else showUsage
        _       ->  showUsage