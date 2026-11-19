module Main (main) where
import System.Environment (getArgs, getProgName)
import Data.List (isSuffixOf)
import Compiler (compile)
import Finalizer (finalize)
import Text.Read (readMaybe)

version :: String
version = "0.1.0.0"

showUsage :: IO ()
showUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <sourcefile.mcmd> <y>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-v"]  -> putStrLn version
        [path, yStr]
            | ".mcmd" `isSuffixOf` path -> case readMaybe yStr :: Maybe Int of
                Just y  -> do
                    chains <- compile path
                    putStrLn $ finalize y chains
                Nothing -> showUsage
            | otherwise                 -> showUsage
        _       ->  showUsage