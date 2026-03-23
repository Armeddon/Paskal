module Paskal (interpret) where
import Control.Monad.Writer (runWriter)
import Token
import Lexer
import Parser
import Executor
import Util

interpret :: String -> IO ()
interpret = either printInvalidTokens interpretTokens . tokenize

printInvalidTokens :: [Pos String] -> IO ()
printInvalidTokens = mapM_ $ putStrLn . (\(p, t) -> formatError p $ "Invalid token: " ++ t)

interpretTokens :: [Pos Token] -> IO ()
interpretTokens tokens = let (ast, logs) = runWriter $ parse tokens
    in mapM_ putStrLn logs >> (sequenceA . fmap execute) ast >> return ()
