module Main where

import Paskal
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, IOException)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr $ "No file argument was provided"
        (arg:_) -> interpretFile arg

interpretFile :: FilePath -> IO ()
interpretFile filename = do
    contents <- try $ readFile filename :: IO (Either IOException String)
    case contents of
        Left _ -> hPutStrLn stderr $ "Couldn't open the file " ++ filename
        Right text -> interpret (Filename filename) text
