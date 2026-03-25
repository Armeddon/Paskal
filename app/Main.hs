module Main where

import Paskal
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    interpret (Filename filename) contents
