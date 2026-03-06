module Paskal (interpret) where

import Lexer
import Parser
import Executor

interpret :: String -> IO ()
interpret = execute . parse . tokenize
