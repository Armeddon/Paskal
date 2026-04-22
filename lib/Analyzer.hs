module Analyzer where
import AST
import Control.Monad.Writer (Writer)
import Util

analyze :: AST -> Writer [String] ()
analyze (AST _ (Pos _ vars) (Pos _ ops)) = let
    tpes = getVarTypes vars
    varDefPositions = getVarDefPositions vars
    names = map fst tpes
    OperatorSection operators = ops
    in do
        checkVariableUniqueness varDefPositions
        checkUndeclaredIdentifiers names operators

getVarTypes :: VariableSection -> [(Name, Pos Type)]
getVarTypes (VariableSection vars) = vars >>= \(names, tpe) -> [(n, tpe) | Pos _ n <- names]

getVarDefPositions :: VariableSection -> [Pos Name]
getVarDefPositions (VariableSection vars) = vars >>= fst

checkVariableUniqueness :: [Pos Name] -> Writer [String] ()
checkVariableUniqueness [] = return ()
checkVariableUniqueness (Pos pos name : vars) = let
    duplicates = filter (\(Pos _ n) -> n == name) vars
    string = case name of { Name s -> s }
    in if null duplicates
       then checkVariableUniqueness vars
       else do
       tellError pos $ "The variable " ++ string ++ " is declared several times"
       mapM_ (\(Pos p _) -> tellError p "Duplicate variable declaration") duplicates
       checkVariableUniqueness $ filter (not . (`elem` duplicates)) vars

checkUndeclaredIdentifiers :: [Name] -> [Pos Operator] -> Writer [String] ()
checkUndeclaredIdentifiers _ [] = return ()
checkUndeclaredIdentifiers names (Pos _ (AssignmentOperator target expr) : ops) = let
    Pos targetPos (Name targetString) = target
    checkTarget = if Name targetString `elem` names
        then return ()
        else tellError targetPos $ "Undeclared variable " ++ show targetString
    in do
        checkTarget
        checkUndeclIdentExpr names expr
        checkUndeclaredIdentifiers names ops
checkUndeclaredIdentifiers names (Pos _ (OutputOperator exprs) : ops) = do
    mapM_ (checkUndeclIdentExpr names) exprs
    checkUndeclaredIdentifiers names ops
checkUndeclaredIdentifiers names (Pos _ (CompoundOperator operators) : ops) = checkUndeclaredIdentifiers names $ operators ++ ops
checkUndeclaredIdentifiers names (Pos _ (WhileLoopOperator expr op) : ops) = do
    checkUndeclIdentExpr names expr
    checkUndeclaredIdentifiers names $ op : ops
checkUndeclaredIdentifiers names (Pos _ (SwitchOperator expr variants) : ops) = do
    checkUndeclIdentExpr names expr
    checkUndeclaredIdentifiers names $ map snd variants ++ ops
checkUndeclaredIdentifiers names (Pos _ (IfOperator expr thenOp elseOp) : ops) = do
    checkUndeclIdentExpr names expr
    checkUndeclaredIdentifiers names $ thenOp : elseOp : ops

checkUndeclIdentExpr :: [Name] -> Pos Expression -> Writer [String] ()
checkUndeclIdentExpr names (Pos _ (SimpleExpression sumExpr)) = checkUndeclIdentSumExpr names sumExpr
checkUndeclIdentExpr names (Pos _ (RelationExpression _ sum1 sum2)) = do
    checkUndeclIdentSumExpr names sum1
    checkUndeclIdentSumExpr names sum2

checkUndeclIdentSumExpr :: [Name] -> Pos SumExpression -> Writer [String] () 
checkUndeclIdentSumExpr names (Pos _ (SimpleSumExpression prod)) = checkUndeclIdentProdExpr names prod
checkUndeclIdentSumExpr names (Pos _ (AdditionExpression _ prod1 prod2)) = do
    checkUndeclIdentSumExpr names prod1
    checkUndeclIdentProdExpr names prod2

checkUndeclIdentProdExpr :: [Name] -> Pos ProductExpression -> Writer [String] ()
checkUndeclIdentProdExpr names (Pos _ (SimpleProductExpression basic)) = checkUndeclIdentBasicExpr names basic
checkUndeclIdentProdExpr names (Pos _ (MultiplicationExpression _ basic1 basic2)) = do
    checkUndeclIdentProdExpr names basic1
    checkUndeclIdentBasicExpr names basic2

checkUndeclIdentBasicExpr :: [Name] -> Pos BasicExpression -> Writer [String] ()
checkUndeclIdentBasicExpr names (Pos _ (NameExpression (Pos pos name@(Name string)))) =
    if name `elem` names
    then return ()
    else tellError pos $ "Undeclared variable " ++ string
checkUndeclIdentBasicExpr _ _ = return ()

checkOperator :: [(Name, Type)] -> [(Name, Constant)] -> Pos Operator -> Writer [String] (Maybe Operator)
checkOperator = undefined
