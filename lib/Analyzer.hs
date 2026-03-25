module Analyzer where
import AST
import Control.Monad.Writer (Writer)

analyze :: AST -> Writer [String] (Maybe AST)
analyze = return . Just
