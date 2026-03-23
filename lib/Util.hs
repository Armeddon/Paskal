module Util where
import Control.Monad.Writer (Writer, tell)

formatError :: (Int, Int) -> String -> String
formatError (row, column) message = concat $ [show row, ":", show column, ": ", message]

tellError :: (Int, Int) -> String -> Writer [String] ()
tellError position = tell . (:[]) . formatError position

type Pos a = ((Int, Int), a)
