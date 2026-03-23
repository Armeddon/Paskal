module Executor (execute) where
import AST

import Data.List (find)
import Data.Maybe (isJust)
import Data.Either.Extra (maybeToEither)

type Memory = [(Name, Constant)]
type VariableTypes = [(Name, Type)]
type ErrorProne = Either (Maybe String)

execute :: AST -> IO ()
execute (AST _ (VariableSection vars) (OperatorSection ops)) = executeOperators (vars >>= \(names, tpe) -> [(n, tpe) | n <- names]) [] ops

executeOperators :: VariableTypes -> Memory -> [Operator] -> IO ()
executeOperators _ _ [] = return ()
executeOperators vt m (op:ops) = do
    m' <- executeOperator vt m op
    executeOperators vt m' ops

executeOperator :: VariableTypes -> Memory -> Operator -> IO Memory
executeOperator vt m (AssignmentOperator name expr) = do
    let result = evaluateExpression expr m
    case result of
        Left msg -> printError msg >> return m
        Right value -> return $ do
            if isJust $ find (typeOf value ==) $ lookup name vt
            then (name, value) : filter ((/= name) . fst) m
            else m
executeOperator _ m (OutputOperator []) = return m
executeOperator vt m (OutputOperator (expr:exprs)) = do
    let result = evaluateExpression expr m
    case result of
        Left msg -> printError msg >> return m
        Right value -> printConstant value >> executeOperator vt m (OutputOperator exprs)
executeOperator _ m _ = undefined

evaluateExpression :: Expression -> Memory -> ErrorProne Constant
evaluateExpression (SimpleExpression expr) = evaluateSumExpression expr
evaluateExpression (RelationExpression op e1 e2) = evaluateRelationExpression op e1 e2

evaluateSumExpression :: SumExpression -> Memory -> ErrorProne Constant
evaluateSumExpression (SimpleSumExpression expr) m = evaluateProductExpression expr m
evaluateSumExpression (AdditionExpression op e1 e2) m = do
    c1 <- evaluateSumExpression e1 m
    c2 <- evaluateProductExpression e2 m
    evaluateSumResult op (c1, c2)
    where evaluateSumResult Add (IntegerConstant a, IntegerConstant b) = return . IntegerConstant $ a + b
          evaluateSumResult Subtract (IntegerConstant a, IntegerConstant b) = return . IntegerConstant $ a - b
          evaluateSumResult Or (BooleanConstant a, BooleanConstant b) = return . BooleanConstant $ a || b
          evaluateSumResult _ _ = Left Nothing

evaluateRelationExpression :: RelationOperation -> SumExpression -> SumExpression -> Memory -> ErrorProne Constant
evaluateRelationExpression op e1 e2 m = do
    c1 <- evaluateSumExpression e1 m
    c2 <- evaluateSumExpression e2 m
    evaluateRelationResult op (c1, c2)
    where evaluateRelationResult Greater (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a > b
          evaluateRelationResult Less (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a < b
          evaluateRelationResult NotEqual (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a /= b
          evaluateRelationResult NotEqual (BooleanConstant a, BooleanConstant b) = return . BooleanConstant $ a /= b
          evaluateRelationResult LessOrEqual (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a <= b
          evaluateRelationResult GreaterOrEqual (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a >= b
          evaluateRelationResult Equal (IntegerConstant a, IntegerConstant b) = return . BooleanConstant $ a == b
          evaluateRelationResult Equal (BooleanConstant a, BooleanConstant b) = return . BooleanConstant $ a == b
          evaluateRelationResult _ _ = Left Nothing

evaluateProductExpression :: ProductExpression -> Memory -> ErrorProne Constant
evaluateProductExpression (SimpleProductExpression expr) m = evaluateBasicExpression expr m
evaluateProductExpression (MultiplicationExpression op e1 e2) m = do
    c1 <- evaluateProductExpression e1 m
    c2 <- evaluateBasicExpression e2 m
    evaluateProductResult op (c1, c2)
    where evaluateProductResult Multiply (IntegerConstant a, IntegerConstant b) = return . IntegerConstant $ a * b
          evaluateProductResult Divide (IntegerConstant _, IntegerConstant 0) = Left $ Just "Division by zero"
          evaluateProductResult Divide (IntegerConstant a, IntegerConstant b) = return . IntegerConstant $ a `div` b
          evaluateProductResult Modulus (IntegerConstant _, IntegerConstant 0) = Left $ Just "Division by zero"
          evaluateProductResult Modulus (IntegerConstant a, IntegerConstant b) = return . IntegerConstant $ a `mod` b
          evaluateProductResult And (BooleanConstant a, BooleanConstant b) = return . BooleanConstant $ a && b
          evaluateProductResult _ _ = Left Nothing

evaluateBasicExpression :: BasicExpression -> Memory -> ErrorProne Constant
evaluateBasicExpression (NameExpression name) m = maybeToEither Nothing $ lookup name m
evaluateBasicExpression (ConstantExpression c) _ = return c
evaluateBasicExpression (ParenthesizedExpression expr) m = evaluateExpression expr m
evaluateBasicExpression (NotExpression expr) m = do
    c <- evaluateBasicExpression expr m
    case c of
        BooleanConstant a -> return . BooleanConstant $ not a 
        IntegerConstant _ -> Left Nothing

printError :: Maybe String -> IO ()
printError = putStrLn . maybe "Unknown error occured" id

printConstant :: Constant -> IO ()
printConstant (IntegerConstant x) = print x
printConstant (BooleanConstant x) = print x
