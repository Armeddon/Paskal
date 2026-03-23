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
parse ((_, ProgramKeyword) : (_, Identifier name) : (_, Semicolon) : rest) = do
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST (Name name)) <*> variableSection <*> operatorSection
parse ((_, ProgramKeyword) : (_, Identifier name) : (pos, _) : rest) = do
    tellError pos "Expected a semicolon after the program name"
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST (Name name)) <*> variableSection <*> operatorSection
parse ((_, ProgramKeyword) : (pos, _) : _) = do
    tellError pos "Expected the program name after the program keyword"
    return Nothing
parse ((pos, _) : _) = do
    tellError pos "Expected the program to start with the program keyword"
    return Nothing
parse [] = do
    tellError (0, 0) "Expected the program to start with the program keyword"
    return Nothing

parseVariableSection :: [Pos Token] -> Writer [String] (Maybe VariableSection, [Pos Token])
parseVariableSection ((_, VarKeyword):rest) = do
    (names, rest') <- parseVariableDefinition rest
    case rest' of
        ((_, Semicolon):b@(_, BeginKeyword):rest'') -> return (VariableSection . (:[]) <$> names, b:rest'')
        ((_, Semicolon):i@(_, Identifier _):rest'') -> do
            (section, rest''') <- parseVariableSection $ ((0,0), VarKeyword):i:rest''
            return $ (section >>= \(VariableSection s) -> VariableSection . (:s) <$> names, rest''')
        ((_, Semicolon):(pos, token):rest'') -> do
            tellError pos $ "Expected either the begin keyword or an identifier, found " ++ show token
            return (Nothing, (pos, token):rest'')
        ((pos, token):rest'') -> do
            tellError pos $ "Expected a semicolon, found " ++ show token
            return (Nothing, (pos, token):rest'')
        [] -> return (Nothing, [])
parseVariableSection ((pos, _):rest) = tellError pos "Expected the variable declaration section to begin with the var keyword" >> return (Nothing, rest)
parseVariableSection [] = tellError (0, 0) "Expected the variable declaration section to begin with the var keyword" >> return (Nothing, [])

parseVariableDefinition :: [Pos Token] -> Writer [String] (Maybe ([Name], Type), [Pos Token])
parseVariableDefinition ((_, Identifier name) : (_, Comma) : i@(_, Identifier _) : rest) = do
    (names, rest') <- parseVariableDefinition $ i : rest
    return $ (fmap (\(n, t) -> (Name name:n, t)) names, rest')
parseVariableDefinition ((_, Identifier _) : (_, Comma) : (pos, token) : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, (pos, token) : rest)
parseVariableDefinition ((_, Identifier name) : (_, Colon) : (_, IntegerType) : rest) = return (Just ([Name name], AST.IntegerType), rest)
parseVariableDefinition ((_, Identifier name) : (_, Colon) : (_, BooleanType) : rest) = return (Just ([Name name], AST.BooleanType), rest)
parseVariableDefinition ((_, Identifier _) : (_, Colon) : (pos, token) : rest) = do
    tellError pos $ "Expected a type. Actual: " ++ show token
    return (Nothing, (pos, token) : rest)
parseVariableDefinition ((_, Identifier _) : (pos, token) : rest) = do
    tellError pos $ "Expected a comma or a colon. Actual: " ++ show token
    return (Nothing, (pos, token) : rest)
parseVariableDefinition ((pos, token) : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, (pos, token) : rest)
parseVariableDefinition [] = return (Nothing, [])

parseOperatorSection :: [Pos Token] -> Writer [String] (Maybe OperatorSection)
parseOperatorSection ((_, BeginKeyword) : i@(_, Identifier _): rest) = (parseAssignmentOperator $ i : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : w@(_, WritelnKeyword): rest) = (parseOutputOperator $ w : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : b@(_, BeginKeyword): rest) = (parseCompoundOperator $ b : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : w@(_, WhileKeyword): rest) = (parseWhileLoopOperator $ w : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : c@(_, CaseKeyword): rest) = (parseSwitchOperator $ c : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : i@(_, IfKeyword): rest) = (parseIfOperator $ i : rest) >>= uncurry parseOperatorSectionRest
parseOperatorSection ((_, BeginKeyword) : (pos, token) : _) = do
    tellError pos $ "Expected an operator. Actual: " ++ show token
    return Nothing
parseOperatorSection ((pos, token) : _) = do
    tellError pos $ "Expected the begin keyword at the beginning of the operator section. Actual: " ++ show token
    return Nothing
parseOperatorSection [] = return Nothing

parseOperatorSectionRest :: Maybe Operator -> [Pos Token] -> Writer [String] (Maybe OperatorSection)
parseOperatorSectionRest op rest = case rest of
    ((_, EndKeyword) : (_, Dot) : (_, EOF) : _) -> return $ OperatorSection . (:[]) <$> op 
    ((_, EndKeyword) : (_, Dot) : (pos, token) : _) -> do
        tellError pos $ "Expected end of file after the end of program. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    ((_, EndKeyword) : (pos, token) : _) -> do
        tellError pos $ "Expected a dot after the end of the operator section. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    ((pos, Semicolon) : rest') -> do
        section <- parseOperatorSection $ (pos, BeginKeyword) : rest'
        return $ section >>= \(OperatorSection ops) -> OperatorSection . (:ops) <$> op
    ((pos, token) : _) -> do
        tellError pos $ "Expected either the end keyword or a semicolon. Actual: " ++ show token
        return Nothing
    [] -> return Nothing

parseAssignmentOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseAssignmentOperator ((_, Identifier name) : (_, AssignmentOperator) : rest) = do
    (expr, rest') <- parseExpression rest
    return (AST.AssignmentOperator (Name name) <$> expr, rest')
parseAssignmentOperator ((_, Identifier _) : (pos, token) : rest) = do
    tellError pos $ "Expected an assignment operator (:=). Actual" ++ show token
    return (Nothing, (pos, token) : rest)
parseAssignmentOperator tokens = return (Nothing, tokens)

parseOutputOperator :: [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseOutputOperator ((_, WritelnKeyword): (_, OpenParen): rest) = do
    (args, rest') <- parseArgumentList rest
    case args of
        Just args' -> parseOutputOperatorRest args' rest'
        Nothing -> return (Nothing, rest')
parseOutputOperator ((_, WritelnKeyword): (pos, token): rest) = do
    tellError pos $ "Expected an opening parenthesis for the writeln function call. Actual: " ++ show token
    return (Nothing, (pos, token) : rest)
parseOutputOperator tokens = return (Nothing, tokens)

parseOutputOperatorRest :: [Expression] -> [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseOutputOperatorRest args ((_, CloseParen) : rest) = return (Just $ OutputOperator args, rest)
parseOutputOperatorRest args ((pos, token) : rest) = do
    when (not $ null args) $ tellError pos $ "Expected a closing parenthesis. Actual: " ++ show token
    return (Nothing, rest)
parseOutputOperatorRest _ [] = return (Nothing, [])
     
parseArgumentList :: [Pos Token] -> Writer [String] (Maybe [Expression], [Pos Token])
parseArgumentList tokens = do
    (expr, rest) <- parseExpression tokens
    if isJust expr then case rest of
        ((_, Comma) : rest') -> do
            (exprs, rest'') <- parseArgumentList rest'
            return (liftA2 (:) expr exprs, rest'')
        ((_, CloseParen) : _) -> return ((:[]) <$> expr, rest)
        ((pos, token) : _) -> do
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
        (Just expr, (_, t):ts) | isJust $ toRelationOp t -> do
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
        parseSumLoop acc ((_, t):ts) | isJust $ toAdditionOp t = do
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
        parseProductLoop acc ((_, t):ts) | isJust $ toMultiplicationOp t = do
            parsed <- parseBasicExpression ts
            case parsed of
                (Nothing, rest) -> return (Nothing, rest)
                (Just expr, rest) -> let
                    newAcc = MultiplicationExpression (fromJust $ toMultiplicationOp t) acc expr
                    in parseProductLoop newAcc rest
        parseProductLoop acc tokens' = return (Just acc, tokens')

parseBasicExpression :: [Pos Token] -> Writer [String] (Maybe BasicExpression, [Pos Token])
parseBasicExpression [] = return (Nothing, [])
parseBasicExpression ((_, token):rest) = case token of
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
            (Just expr, ((_, CloseParen):rest')) -> (Just $ ParenthesizedExpression expr, rest')
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
