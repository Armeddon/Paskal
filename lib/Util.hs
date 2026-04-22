module Util where
import Control.Monad.Writer (Writer, tell)

formatError :: (Filename, Int, Int) -> String -> String
formatError (Filename file, row, column) message = concat [
        file,
        ":",
        show row,
        ":",
        show column,
        ": error: ",
        message
    ]

tellError :: (Filename, Int, Int) -> String -> Writer [String] ()
tellError position = tell . (:[]) . formatError position

data Pos a = Pos (Filename, Int, Int) a deriving Eq

newtype Filename = Filename String deriving Eq
