module Parser (parse) where
import Token
import AST hiding (Type (..), Operator (AssignmentOperator), Constant (..))
import qualified AST (Type (..), Operator (AssignmentOperator), Constant (..))
import AST (Type, Operator)
import Util

import Control.Monad.Writer (Writer)
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)

parse :: [Pos Token] -> Writer [String] (Maybe AST)
parse ((Pos _ ProgramKeyword) : (Pos _ (Identifier name)) : (Pos _ Semicolon) : rest) = do
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST (Name name)) <*> variableSection <*> operatorSection
parse ((Pos _ ProgramKeyword) : (Pos _ (Identifier name)) : (Pos pos _) : rest) = do
    tellError pos "Expected a semicolon after the program name"
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST (Name name)) <*> variableSection <*> operatorSection
parse ((Pos _ ProgramKeyword) : (Pos pos _) : _) = do
    tellError pos "Expected the program name after the program keyword"
    return Nothing
parse ((Pos pos _) : _) = do
    tellError pos "Expected the program to start with the program keyword"
    return Nothing
parse [] = return Nothing

parseVariableSection :: [Pos Token] -> Writer [String] (Maybe VariableSection, [Pos Token])
parseVariableSection ((Pos (f, _, _) VarKeyword):rest) = do
    (names, rest') <- parseVariableDefinition rest
    case rest' of
        ((Pos _ Semicolon):b@(Pos _ BeginKeyword):rest'') -> return (VariableSection . (:[]) <$> names, b:rest'')
        ((Pos _ Semicolon):i@(Pos _ (Identifier _)):rest'') -> do
            (section, rest''') <- parseVariableSection $ (Pos (f, 0, 0) VarKeyword):i:rest''
            return $ (section >>= \(VariableSection s) -> VariableSection . (:s) <$> names, rest''')
        ((Pos _ Semicolon):(Pos pos token):rest'') -> do
            tellError pos $ "Expected either the begin keyword or an identifier, found " ++ show token
            return (Nothing, (Pos pos token):rest'')
        ((Pos pos token):rest'') -> do
            tellError pos $ "Expected a semicolon, found " ++ show token
            return (Nothing, (Pos pos token):rest'')
        [] -> return (Nothing, [])
parseVariableSection ((Pos pos _):rest) = do
    tellError pos "Expected the variable declaration section to begin with the var keyword"
    return (Nothing, rest)
parseVariableSection [] = return (Nothing, [])

parseVariableDefinition :: [Pos Token] -> Writer [String] (Maybe ([Name], Type), [Pos Token])
parseVariableDefinition ((Pos _ (Identifier name)) : (Pos _ Comma) : i@(Pos _ (Identifier _)) : rest) = do
    (names, rest') <- parseVariableDefinition $ i : rest
    return $ (fmap (\(n, t) -> (Name name:n, t)) names, rest')
parseVariableDefinition ((Pos _ (Identifier _)) : (Pos _ Comma) : (Pos pos token) : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, (Pos pos token) : rest)
parseVariableDefinition ((Pos _ (Identifier name)) : (Pos _ Colon) : (Pos _ IntegerType) : rest) = return (Just ([Name name], AST.IntegerType), rest)
parseVariableDefinition ((Pos _ (Identifier name)) : (Pos _ Colon) : (Pos _ BooleanType) : rest) = return (Just ([Name name], AST.BooleanType), rest)
parseVariableDefinition ((Pos _ (Identifier _)) : (Pos _ Colon) : (Pos pos token) : rest) = do
    tellError pos $ "Expected a type. Actual: " ++ show token
    return (Nothing, (Pos pos token) : rest)
parseVariableDefinition ((Pos _ (Identifier _)) : (Pos pos token) : rest) = do
    tellError pos $ "Expected a comma or a colon. Actual: " ++ show token
    return (Nothing, (Pos pos token) : rest)
parseVariableDefinition ((Pos pos token) : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, (Pos pos token) : rest)
parseVariableDefinition [] = return (Nothing, [])

parseOperatorSection :: [Pos Token] -> Writer [String] (Maybe OperatorSection)
parseOperatorSection ((Pos _ BeginKeyword) : i@(Pos _ (Identifier _)): rest) = (parseAssignmentOperator $ i : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : w@(Pos _ WritelnKeyword): rest) = (parseOutputOperator $ w : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : b@(Pos _ BeginKeyword): rest) = (parseCompoundOperator $ b : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : w@(Pos _ WhileKeyword): rest) = (parseWhileLoopOperator $ w : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : c@(Pos _ CaseKeyword): rest) = (parseSwitchOperator $ c : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : i@(Pos _ IfKeyword): rest) = (parseIfOperator $ i : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((Pos _ BeginKeyword) : (Pos pos token) : _) = do
    tellError pos $ "Expected an operator. Actual: " ++ show token
    return Nothing
parseOperatorSection ((Pos pos token) : _) = do
    tellError pos $ "Expected the begin keyword at the beginning of the operator section. Actual: " ++ show token
    return Nothing
parseOperatorSection [] = return Nothing

parseOperatorSectionRest :: Maybe Operator -> [Pos Token] -> Writer [String] (Maybe OperatorSection)
parseOperatorSectionRest op rest = case rest of
    ((Pos _ EndKeyword) : (Pos _ Dot) : (Pos _ EOF) : _) -> return $ OperatorSection . (:[]) <$> op 
    ((Pos _ EndKeyword) : (Pos _ Dot) : (Pos pos token) : _) -> do
        tellError pos $ "Expected end of file after the end of program. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    ((Pos _ EndKeyword) : (Pos pos token) : _) -> do
        tellError pos $ "Expected a dot after the end of the operator section. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    ((Pos pos Semicolon) : rest') -> do
        section <- parseOperatorSection $ (Pos pos BeginKeyword) : rest'
        return $ section >>= \(OperatorSection ops) -> OperatorSection . (:ops) <$> op
    ((Pos pos token) : _) -> do
        tellError pos $ "Expected either the end keyword or a semicolon. Actual: " ++ show token
        return Nothing
    [] -> return Nothing

parseAssignmentOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseAssignmentOperator ((Pos _ (Identifier name)) : (Pos _ AssignmentOperator) : rest) = do
    (expr, rest') <- parseExpression rest
    return (AST.AssignmentOperator (Name name) <$> expr, rest')
parseAssignmentOperator ((Pos _ (Identifier _)) : (Pos pos token) : rest) = do
    tellError pos $ "Expected an assignment operator (:=). Actual" ++ show token
    return (Nothing, (Pos pos token) : rest)
parseAssignmentOperator tokens = return (Nothing, tokens)

parseOutputOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseOutputOperator ((Pos _ WritelnKeyword): (Pos _ OpenParen): rest) = do
    (args, rest') <- parseArgumentList rest
    case args of
        Just args' -> parseOutputOperatorRest args' rest'
        Nothing -> return (Nothing, rest')
parseOutputOperator ((Pos _ WritelnKeyword): (Pos pos token): rest) = do
    tellError pos $ "Expected an opening parenthesis for the writeln function call. Actual: " ++ show token
    return (Nothing, (Pos pos token) : rest)
parseOutputOperator tokens = return (Nothing, tokens)

parseOutputOperatorRest :: [Expression] -> [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseOutputOperatorRest args ((Pos _ CloseParen) : rest) = return (Just $ OutputOperator args, rest)
parseOutputOperatorRest args ((Pos pos token) : rest) = do
    when (not $ null args) $ tellError pos $ "Expected a closing parenthesis. Actual: " ++ show token
    return (Nothing, rest)
parseOutputOperatorRest _ [] = return (Nothing, [])
     
parseArgumentList :: [Pos Token] -> Writer [String] (Maybe [Expression], [Pos Token])
parseArgumentList tokens = do
    (expr, rest) <- parseExpression tokens
    if isJust expr then case rest of
        ((Pos _ Comma) : rest') -> do
            (exprs, rest'') <- parseArgumentList rest'
            return (liftA2 (:) expr exprs, rest'')
        ((Pos _ CloseParen) : _) -> return ((:[]) <$> expr, rest)
        ((Pos pos token) : _) -> do
            tellError pos $ "Expected either a comma or a closing parenthesis after the function argument. Actual: " ++ show token
            return (Nothing, rest)
        [] -> return (Nothing, rest)
    else return (Nothing, rest)

parseCompoundOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseCompoundOperator = undefined

parseWhileLoopOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseWhileLoopOperator = undefined

parseSwitchOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseSwitchOperator = undefined

parseIfOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseIfOperator = undefined

parseExpression :: [Pos Token] -> Writer [String] (Maybe Expression, [Pos Token])
parseExpression tokens = do
    parsed <- parseSumExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just expr, (Pos _ t):ts) | isJust $ toRelationOp t -> do
            parsed' <- parseSumExpression ts
            return $ case parsed' of
                (Nothing, rest) -> (Nothing, rest)
                (Just expr', rest) ->
                    (pure RelationExpression <*> toRelationOp t <*> pure expr <*> pure expr', rest)
        _ -> return (SimpleExpression <$> fst parsed, snd parsed)

parseSumExpression :: [Pos Token] -> Writer [String] (Maybe SumExpression, [Pos Token])
parseSumExpression tokens = do
    parsed <- parseProductExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just prod, rest) -> parseSumLoop (SimpleSumExpression prod) rest
    where
        parseSumLoop acc ((Pos _ t):ts) | isJust $ toAdditionOp t = do
            parsed <- parseProductExpression ts
            case parsed of
                (Nothing, rest) -> return (Nothing, rest)
                (Just expr, rest) -> let
                    newAcc = AdditionExpression (fromJust $ toAdditionOp t) acc expr
                    in parseSumLoop newAcc rest
        parseSumLoop acc tokens' = return (Just acc, tokens')
        
parseProductExpression :: [Pos Token] -> Writer [String] (Maybe ProductExpression, [Pos Token])
parseProductExpression tokens = do
    parsed <- parseBasicExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just basic, rest) -> parseProductLoop (SimpleProductExpression basic) rest
    where
        parseProductLoop acc ((Pos _ t):ts) | isJust $ toMultiplicationOp t = do
            parsed <- parseBasicExpression ts
            case parsed of
                (Nothing, rest) -> return (Nothing, rest)
                (Just expr, rest) -> let
                    newAcc = MultiplicationExpression (fromJust $ toMultiplicationOp t) acc expr
                    in parseProductLoop newAcc rest
        parseProductLoop acc tokens' = return (Just acc, tokens')

parseBasicExpression :: [Pos Token] -> Writer [String] (Maybe BasicExpression, [Pos Token])
parseBasicExpression [] = return (Nothing, [])
parseBasicExpression ((Pos _ token):rest) = case token of
    Identifier name -> return (Just . NameExpression $ Name name, rest)
    IntegerConstant i -> return (Just . ConstantExpression $ AST.IntegerConstant i, rest)
    BooleanConstant b -> return (Just . ConstantExpression $ AST.BooleanConstant b, rest)
    NotOperator -> do
        parsed <- parseBasicExpression rest
        return $ case parsed of
            (Nothing, rest') -> (Nothing, rest')
            (Just basic, rest') -> (Just $ NotExpression basic, rest')
    OpenParen -> do
        parsed <- parseExpression rest
        return $ case parsed of
            (Just expr, ((Pos _ CloseParen):rest')) -> (Just $ ParenthesizedExpression expr, rest')
            (_, rest') -> (Nothing, rest')
    _ -> return (Nothing, rest)

toRelationOp :: Token -> Maybe RelationOperation
toRelationOp GreaterOperator = Just Greater 
toRelationOp LessOperator = Just Less 
toRelationOp NotEqualOperator = Just NotEqual 
toRelationOp LessOrEqualOperator = Just LessOrEqual 
toRelationOp GreaterOrEqualOperator = Just GreaterOrEqual 
toRelationOp EqualOperator = Just Equal 
toRelationOp _ = Nothing

toAdditionOp :: Token -> Maybe AdditionOperation
toAdditionOp AddOperator = Just Add
toAdditionOp SubtractOperator = Just Subtract
toAdditionOp OrOperator = Just Or
toAdditionOp _ = Nothing

toMultiplicationOp :: Token -> Maybe MultiplicationOperation
toMultiplicationOp MultiplyOperator = Just Multiply
toMultiplicationOp DivideOperator = Just Divide
toMultiplicationOp ModulusOperator = Just Modulus
toMultiplicationOp AndOperator = Just And
toMultiplicationOp _ = Nothing
