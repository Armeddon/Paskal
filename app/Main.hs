module Main where

import Paskal

main :: IO ()
main = getContents >>= interpret
