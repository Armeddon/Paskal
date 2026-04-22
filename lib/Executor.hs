module Executor (execute) where
import AST
import Util

import Data.List (find)
import Data.Maybe (isJust)
import Data.Either.Extra (maybeToEither)

execute :: AST -> IO ()
execute (AST _ (Pos _ (VariableSection vars)) (Pos _ (OperatorSection ops))) =
    executeOperators (vars >>= \(names, Pos _ tpe) -> [(n, tpe) | Pos _ n <- names]) [] ops

executeOperators :: [(Name, Type)] -> [(Name, Constant)] -> [Pos Operator] -> IO ()
executeOperators _ _ [] = return ()
executeOperators vt m (op:ops) = do
    m' <- executeOperator vt m op
    executeOperators vt m' ops

executeOperator :: [(Name, Type)] -> [(Name, Constant)] -> Pos Operator -> IO [(Name, Constant)]
executeOperator vt m (Pos _ (AssignmentOperator (Pos _ name) expr)) = do
    let result = evaluateExpression expr m
    case result of
        Left (Pos epos msg) -> printError epos msg >> return m
        Right value -> return $ do
            if isJust $ find (typeOfConstant value ==) $ lookup name vt
            then (name, value) : filter ((/= name) . fst) m
            else m
executeOperator _ m (Pos _ (OutputOperator [])) = return m
executeOperator vt m (Pos pos (OutputOperator (expr:exprs))) = do
    let result = evaluateExpression expr m
    case result of
        Left (Pos epos msg) -> printError epos msg >> return m
        Right value -> printConstant value >> executeOperator vt m (Pos pos $ OutputOperator exprs)
executeOperator vt m (Pos _ (CompoundOperator ops)) =
    foldl (flip (.) (flip $ executeOperator vt) . (>>=)) (return m) ops
executeOperator vt m w@(Pos _ (WhileLoopOperator cond@(Pos pos _) op)) = let
    expr = evaluateExpression cond m
    in case expr of
        Left (Pos pos' msg) -> do
            printError pos' msg
            return m
        Right (BooleanConstant True) -> do
            m' <- executeOperator vt m op
            executeOperator vt m' w
        Right (BooleanConstant False) -> return m
        Right (IntegerConstant i) -> do
            printError pos . Just $
                "Expected a boolean constant, found an integer constant " ++ show i
            return m
executeOperator _ m (Pos _ (SwitchOperator _ [])) = return m
executeOperator vt m (Pos pos (SwitchOperator expr (var:vars))) = let
    res = evaluateExpression expr m
    in case res of
        Left (Pos pos' msg) -> do
            printError pos' msg
            return m
        Right (BooleanConstant b1) -> case fst var of
            Pos _ (BooleanConstant b2) -> executeOperator vt m $
                if b1 == b2
                then snd var
                else Pos pos (SwitchOperator expr vars)
            Pos _ (IntegerConstant _) -> executeOperator vt m (Pos pos (SwitchOperator expr vars))
        Right (IntegerConstant i1) -> case fst var of
            Pos _ (IntegerConstant i2) -> executeOperator vt m $
                if i1 == i2
                then snd var
                else Pos pos (SwitchOperator expr vars)
            Pos _ (BooleanConstant _) -> executeOperator vt m (Pos pos (SwitchOperator expr vars))
executeOperator vt m (Pos _ (IfOperator expr@(Pos pos _) thenOp elseOp)) = let
    res = evaluateExpression expr m
    in case res of
        Left (Pos pos' msg) -> do
            printError pos' msg
            return m
        Right (BooleanConstant True) ->
            executeOperator vt m thenOp
        Right (BooleanConstant False) ->
            executeOperator vt m elseOp
        Right (IntegerConstant i) -> do
            printError pos . Just $ "Expected a boolean expression, found integer result " ++ show i
            return m

evaluateExpression :: Pos Expression -> [(Name, Constant)] -> Either (Pos (Maybe String)) Constant
evaluateExpression (Pos _ (SimpleExpression expr)) = evaluateSumExpression expr
evaluateExpression (Pos _ (RelationExpression op e1 e2)) = evaluateRelationExpression op e1 e2

evaluateSumExpression :: Pos SumExpression -> [(Name, Constant)] ->
    Either (Pos (Maybe String)) Constant
evaluateSumExpression (Pos _ (SimpleSumExpression expr)) m = evaluateProductExpression expr m
evaluateSumExpression (Pos _ (AdditionExpression op e1 e2)) m = do
    c1 <- evaluateSumExpression e1 m
    c2 <- evaluateProductExpression e2 m
    evaluateSumResult op (c1, c2)
    where evaluateSumResult (Pos _ Add) (IntegerConstant a, IntegerConstant b) =
              return . IntegerConstant $ a + b
          evaluateSumResult (Pos _ Subtract) (IntegerConstant a, IntegerConstant b) =
              return . IntegerConstant $ a - b
          evaluateSumResult (Pos _ Or) (BooleanConstant a, BooleanConstant b) =
              return . BooleanConstant $ a || b
          evaluateSumResult (Pos pos Add) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError AddBinOp (typeOfConstant lhs)
                                                                             (typeOfConstant rhs)
          evaluateSumResult (Pos pos Subtract) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError SubtractBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateSumResult (Pos pos Or) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError OrBinOp (typeOfConstant lhs)
                                                                            (typeOfConstant rhs)

evaluateRelationExpression :: Pos RelationOperation -> Pos SumExpression -> Pos SumExpression ->
    [(Name, Constant)] -> Either (Pos (Maybe String)) Constant
evaluateRelationExpression op e1 e2 m = do
    c1 <- evaluateSumExpression e1 m
    c2 <- evaluateSumExpression e2 m
    evaluateRelationResult op (c1, c2)
    where evaluateRelationResult (Pos _ Greater) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a > b
          evaluateRelationResult (Pos _ Less) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a < b
          evaluateRelationResult (Pos _ NotEqual) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a /= b
          evaluateRelationResult (Pos _ NotEqual) (BooleanConstant a, BooleanConstant b) =
              return . BooleanConstant $ a /= b
          evaluateRelationResult (Pos _ LessOrEqual) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a <= b
          evaluateRelationResult (Pos _ GreaterOrEqual) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a >= b
          evaluateRelationResult (Pos _ Equal) (IntegerConstant a, IntegerConstant b) =
              return . BooleanConstant $ a == b
          evaluateRelationResult (Pos _ Equal) (BooleanConstant a, BooleanConstant b) =
              return . BooleanConstant $ a == b
          evaluateRelationResult (Pos pos Greater) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError GreaterBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateRelationResult (Pos pos Less) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError LessBinOp (typeOfConstant lhs)
                                                                              (typeOfConstant rhs)
          evaluateRelationResult (Pos pos NotEqual) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError NotEqualBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateRelationResult (Pos pos LessOrEqual) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError LessOrEqualBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateRelationResult (Pos pos GreaterOrEqual) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError GreaterOrEqualBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateRelationResult (Pos pos Equal) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError EqualBinOp (typeOfConstant lhs)
                                                                               (typeOfConstant rhs)

evaluateProductExpression :: Pos ProductExpression -> [(Name, Constant)] ->
    Either (Pos (Maybe String)) Constant
evaluateProductExpression (Pos _ (SimpleProductExpression expr)) m = evaluateBasicExpression expr m
evaluateProductExpression (Pos _ (MultiplicationExpression op e1 e2)) m = do
    c1 <- evaluateProductExpression e1 m
    c2 <- evaluateBasicExpression e2 m
    evaluateProductResult op (c1, c2)
    where evaluateProductResult (Pos _ Multiply) (IntegerConstant a, IntegerConstant b) =
              return . IntegerConstant $ a * b
          evaluateProductResult (Pos pos Divide) (IntegerConstant _, IntegerConstant 0) =
              Left . Pos pos $ Just "Division by zero"
          evaluateProductResult (Pos _ Divide) (IntegerConstant a, IntegerConstant b) =
              return . IntegerConstant $ a `div` b
          evaluateProductResult (Pos pos Modulus) (IntegerConstant _, IntegerConstant 0) =
              Left . Pos pos $ Just "Division by zero"
          evaluateProductResult (Pos _ Modulus) (IntegerConstant a, IntegerConstant b) =
              return . IntegerConstant $ a `mod` b
          evaluateProductResult (Pos _ And) (BooleanConstant a, BooleanConstant b) =
              return . BooleanConstant $ a && b
          evaluateProductResult (Pos pos Multiply) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError MultiplyBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateProductResult (Pos pos Divide) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError DivideBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateProductResult (Pos pos Modulus) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError ModulusBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)
          evaluateProductResult (Pos pos And) (lhs, rhs) =
              Left . Pos pos . Just $ formatBinaryOperatorTypeError AndBinOp
                                                                    (typeOfConstant lhs)
                                                                    (typeOfConstant rhs)

evaluateBasicExpression :: Pos BasicExpression -> [(Name, Constant)] ->
    Either (Pos (Maybe String)) Constant
evaluateBasicExpression (Pos pos (NameExpression (Pos _ name))) m =
    maybeToEither (Pos pos Nothing) $ lookup name m
evaluateBasicExpression (Pos _ (ConstantExpression (Pos _ c))) _ = return c
evaluateBasicExpression (Pos _ (ParenthesizedExpression expr)) m = evaluateExpression expr m
evaluateBasicExpression (Pos pos (NotExpression expr)) m = do
    c <- evaluateBasicExpression expr m
    case c of
        BooleanConstant a -> return . BooleanConstant $ not a 
        IntegerConstant _ ->
            Left . Pos pos $
                Just "Unary operator not can't be used with the values of type integer."

printError :: (Filename, Int, Int) -> Maybe String -> IO ()
printError position = putStrLn . formatError position . maybe "Unknown error occured" id

printConstant :: Constant -> IO ()
printConstant (IntegerConstant x) = print x
printConstant (BooleanConstant x) = print x

data BinOp = GreaterBinOp
           | LessBinOp
           | NotEqualBinOp
           | LessOrEqualBinOp
           | GreaterOrEqualBinOp
           | EqualBinOp
           | AddBinOp
           | SubtractBinOp
           | OrBinOp
           | MultiplyBinOp
           | DivideBinOp
           | ModulusBinOp
           | AndBinOp

formatBinaryOperatorTypeError :: BinOp -> Type -> Type -> String
formatBinaryOperatorTypeError op lhsType rhsType = concat [
        "Binary operator ",
        showOp op,
        " can't be used with values of types ",
        showTpe lhsType,
        " and ",
        showTpe rhsType,
        "."]
    where
        showOp GreaterBinOp = "greater (>)"
        showOp LessBinOp = "less (<)"
        showOp NotEqualBinOp = "not equal (<>)"
        showOp LessOrEqualBinOp = "less or equal (<=)"
        showOp GreaterOrEqualBinOp = "greater or equal (>=)"
        showOp EqualBinOp = "equal (=)"
        showOp AddBinOp = "add (+)"
        showOp SubtractBinOp = "subtract (-)"
        showOp OrBinOp =  "or"
        showOp MultiplyBinOp = "multiply (*)"
        showOp DivideBinOp = "divide (div)"
        showOp ModulusBinOp = "modulus (mod)"
        showOp AndBinOp = "and"
        showTpe IntegerType = "integer"
        showTpe BooleanType = "boolean"
