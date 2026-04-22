module Paskal (interpret, Filename (..)) where
import Control.Monad.Writer (runWriter, execWriter)
import Token
import Lexer
import Parser
import Executor
import Analyzer
import Util

interpret :: Filename -> String -> IO ()
interpret file = either printInvalidTokens interpretTokens . tokenize file

printInvalidTokens :: [Pos String] -> IO ()
printInvalidTokens = mapM_ $ putStrLn . \(Pos p t) -> formatError p $ "Invalid token: " ++ t

interpretTokens :: [Pos Token] -> IO ()
interpretTokens tokens = do
    let (ast, logs) = runWriter $ parse tokens
        logs' = maybe [] (execWriter . analyze) ast
    mapM_ putStrLn logs
    mapM_ putStrLn logs'
    _ <- sequenceA $ fmap execute ast
    return ()
