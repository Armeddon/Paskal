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
parse (Pos _ ProgramKeyword : Pos pos (Identifier name) : Pos _ Semicolon : rest) = do
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST . Pos pos $ Name name) <*> variableSection <*> operatorSection
parse (Pos _ ProgramKeyword : Pos namePos (Identifier name) : Pos pos _ : rest) = do
    tellError pos "Expected a semicolon after the program name"
    (variableSection, rest') <- parseVariableSection rest
    operatorSection <- parseOperatorSection rest'
    return $ pure (AST (Pos namePos $ Name name)) <*> variableSection <*> operatorSection
parse (Pos _ ProgramKeyword : Pos pos _ : _) = do
    tellError pos "Expected the program name after the program keyword"
    return Nothing
parse (Pos pos _ : _) = do
    tellError pos "Expected the program to start with the program keyword"
    return Nothing
parse [] = return Nothing

parseVariableSection :: [Pos Token] -> Writer [String] (Maybe (Pos VariableSection), [Pos Token])
parseVariableSection (Pos (f, r, c) VarKeyword : rest) = do
    (names, rest') <- parseVariableDefinition rest
    case rest' of
        (Pos _ Semicolon : b@(Pos _ BeginKeyword) : rest'') -> return (Pos (f, r, c) . VariableSection . (:[]) <$> names, b:rest'')
        (Pos _ Semicolon : i@(Pos _ (Identifier _)) : rest'') -> do
            (section, rest''') <- parseVariableSection $ (Pos (f, 0, 0) VarKeyword):i:rest''
            return $ (section >>= \(Pos _ (VariableSection s)) -> Pos (f, r, c) . VariableSection . (:s) <$> names, rest''')
        (Pos _ Semicolon : Pos pos token :rest'') -> do
            tellError pos $ "Expected either the begin keyword or an identifier, found " ++ show token
            return (Nothing, Pos pos token : rest'')
        ((Pos pos token):rest'') -> do
            tellError pos $ "Expected a semicolon, found " ++ show token
            return (Nothing, Pos pos token : rest'')
        [] -> return (Nothing, [])
parseVariableSection (Pos pos _ : rest) = do
    tellError pos "Expected the variable declaration section to begin with the var keyword"
    return (Nothing, rest)
parseVariableSection [] = return (Nothing, [])

parseVariableDefinition :: [Pos Token] -> Writer [String] (Maybe ([Pos Name], Pos Type), [Pos Token])
parseVariableDefinition (Pos pos (Identifier name) : Pos _ Comma : i@(Pos _ (Identifier _)) : rest) = do
    (names, rest') <- parseVariableDefinition $ i : rest
    return $ (fmap (\(n, t) -> (Pos pos (Name name) : n, t)) names, rest')
parseVariableDefinition (Pos _ (Identifier _) : Pos _ Comma : Pos pos token : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, Pos pos token : rest)
parseVariableDefinition (Pos namePos (Identifier name) : Pos _ Colon : Pos typePos IntegerType : rest) = return (Just ([Pos namePos $ Name name], Pos typePos AST.IntegerType), rest)
parseVariableDefinition (Pos namePos (Identifier name) : Pos _ Colon : Pos typePos BooleanType : rest) = return (Just ([Pos namePos $ Name name], Pos typePos AST.BooleanType), rest)
parseVariableDefinition (Pos _ (Identifier _) : Pos _ Colon : Pos pos token : rest) = do
    tellError pos $ "Expected a type. Actual: " ++ show token
    return (Nothing, Pos pos token : rest)
parseVariableDefinition (Pos _ (Identifier _) : Pos pos token : rest) = do
    tellError pos $ "Expected a comma or a colon. Actual: " ++ show token
    return (Nothing, Pos pos token : rest)
parseVariableDefinition (Pos pos token : rest) = do
    tellError pos $ "Expected an identifier. Actual: " ++ show token
    return (Nothing, Pos pos token : rest)
parseVariableDefinition [] = return (Nothing, [])

parseOperatorSection :: [Pos Token] -> Writer [String] (Maybe (Pos OperatorSection))
parseOperatorSection (Pos pos BeginKeyword : rest) = (fmap . fmap) (Pos pos) $ parseOperator rest >>= uncurry parseOperatorSectionRest
parseOperatorSection (Pos pos token : _) = do
    tellError pos $ "Expected the begin keyword at the beginning of the operator section. Actual: " ++ show token
    return Nothing
parseOperatorSection [] = return Nothing

parseOperatorSectionRest :: Maybe (Pos Operator) -> [Pos Token] -> Writer [String] (Maybe OperatorSection)
parseOperatorSectionRest op rest = case rest of
    (Pos _ EndKeyword : Pos _ Dot : Pos _ EOF : _) -> return $ OperatorSection . (:[]) <$> op 
    (Pos _ EndKeyword : Pos _ Dot : Pos pos token : _) -> do
        tellError pos $ "Expected end of file after the end of program. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    (Pos _ EndKeyword : Pos pos token : _) -> do
        tellError pos $ "Expected a dot after the end of the operator section. Actual: " ++ show token
        return $ OperatorSection . (:[]) <$> op
    (Pos semPos Semicolon : rest') -> do
        section <- parseOperatorSection $ Pos semPos BeginKeyword : rest'
        return $ section >>= (\(Pos _ (OperatorSection ops)) -> OperatorSection . (:ops) <$> op)
    (Pos pos token : _) -> do
        tellError pos $ "Expected either the end keyword or a semicolon. Actual: " ++ show token
        return Nothing
    [] -> return Nothing

parseOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseOperator (i@(Pos _ (Identifier _)): rest) = parseAssignmentOperator $ i : rest
parseOperator (w@(Pos _ WritelnKeyword): rest) = parseOutputOperator $ w : rest
parseOperator (b@(Pos _ BeginKeyword): rest) = parseCompoundOperator $ b : rest
parseOperator (w@(Pos _ WhileKeyword): rest) = parseWhileLoopOperator $ w : rest
parseOperator (c@(Pos _ CaseKeyword): rest) = parseSwitchOperator $ c : rest
parseOperator (i@(Pos _ IfKeyword): rest) = parseIfOperator $ i : rest
parseOperator (p@(Pos pos token) : rest) = do
    tellError pos $ "Expected an operator. Actual: " ++ show token
    return (Nothing, p : rest)
parseOperator [] = return (Nothing, [])

parseAssignmentOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseAssignmentOperator (Pos pos (Identifier name) : Pos _ AssignmentOperator : rest) = do
    (expr, rest') <- parseExpression rest
    return (Pos pos . AST.AssignmentOperator (Pos pos $ Name name) <$> expr, rest')
parseAssignmentOperator (Pos _ (Identifier _) : Pos pos token : rest) = do
    tellError pos $ "Expected an assignment operator (:=). Actual" ++ show token
    return (Nothing, Pos pos token : rest)
parseAssignmentOperator tokens = return (Nothing, tokens)

parseOutputOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseOutputOperator (Pos pos WritelnKeyword : Pos _ OpenParen : rest) = do
    (args, rest') <- parseArgumentList rest
    case args of
        Just args' -> (\(op, ts) -> (Pos pos <$> op, ts)) <$> parseOutputOperatorRest args' rest'
        Nothing -> return (Nothing, rest')
parseOutputOperator (Pos _ WritelnKeyword : Pos pos token : rest) = do
    tellError pos $ "Expected an opening parenthesis for the writeln function call. Actual: " ++ show token
    return (Nothing, Pos pos token : rest)
parseOutputOperator tokens = return (Nothing, tokens)

parseOutputOperatorRest :: [Pos Expression] -> [Pos Token] -> Writer [String] (Maybe Operator, [Pos Token])
parseOutputOperatorRest args (Pos _ CloseParen : rest) = return (Just $ OutputOperator args, rest)
parseOutputOperatorRest args (Pos pos token : rest) = do
    when (not $ null args) $ tellError pos $ "Expected a closing parenthesis. Actual: " ++ show token
    return (Nothing, rest)
parseOutputOperatorRest _ [] = return (Nothing, [])
     
parseArgumentList :: [Pos Token] -> Writer [String] (Maybe [Pos Expression], [Pos Token])
parseArgumentList tokens = do
    (expr, rest) <- parseExpression tokens
    if isJust expr then case rest of
        (Pos _ Comma : rest') -> do
            (exprs, rest'') <- parseArgumentList rest'
            return (liftA2 (:) expr exprs, rest'')
        (Pos _ CloseParen : _) -> return ((:[]) <$> expr, rest)
        (Pos pos token : _) -> do
            tellError pos $ "Expected either a comma or a closing parenthesis after the function argument. Actual: " ++ show token
            return (Nothing, rest)
        [] -> return (Nothing, rest)
    else return (Nothing, rest)

parseCompoundOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseCompoundOperator (Pos pos BeginKeyword : rest) = do
    parsed <- parseOperator rest
    case parsed of
        n@(Nothing, _) -> return n
        (Just op, Pos _ EndKeyword : rest') -> return (Just . Pos pos $ CompoundOperator [op], rest')
        (Just op, Pos pos' Semicolon : rest') -> do
            restOps <- parseCompoundOperator $ Pos pos' BeginKeyword : rest'
            case restOps of
                (Just (Pos _ (CompoundOperator ops)), rest'') ->
                     return (Just . Pos pos . CompoundOperator $ op : ops, rest'')
                (_, rest'') -> return (Nothing, rest'')
        (Just op, Pos pos' t : rest') -> do
            tellError pos' $ "Expected either the end keyword or a semicolon, found " ++ show t
            return (Just . Pos pos $ CompoundOperator [op], Pos pos' t : rest')
        (Just _, []) -> return (Nothing, rest)
parseCompoundOperator tokens = return (Nothing, tokens)

parseWhileLoopOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseWhileLoopOperator (Pos pos WhileKeyword : rest) = do
    condition <- parseExpression rest
    case condition of
        (Nothing, rest') -> return (Nothing, rest')
        (Just expr, Pos _ DoKeyword : rest') -> do
            operator <- parseOperator rest'
            case operator of
                n@(Nothing, _) -> return n
                (Just op, rest'') -> return (Just . Pos pos $ WhileLoopOperator expr op, rest'')
        (Just expr, Pos pos' t : rest') -> do
            tellError pos' $ "Expected the do keyword, found " ++ show t
            operator <- parseOperator $ Pos pos' t : rest'
            case operator of
                n@(Nothing, _) -> return n
                (Just op, rest'') -> return (Just . Pos pos $ WhileLoopOperator expr op, rest'')
        (Just _, []) -> return (Nothing, [])
parseWhileLoopOperator (Pos pos t : rest) = do
    tellError pos $ "Expected the while keyword, found " ++ show t
    return (Nothing, Pos pos t : rest)
parseWhileLoopOperator [] = return (Nothing, [])

parseSwitchOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseSwitchOperator (Pos pos CaseKeyword : rest) = do
    expression <- parseExpression rest
    case expression of
        (Nothing, rest') -> return (Nothing, rest')
        (Just expr, Pos _ OfKeyword : rest') -> do
            variant <- parseSwitchVariant rest'
            case variant of
                (Nothing, rest'') -> return (Nothing, rest'')
                (Just var, rest'') -> do
                    variants <- parseSwitchVariantsRest rest''
                    case variants of
                        (Nothing, rest''') -> return (Nothing, rest''')
                        (Just vars, Pos _ EndCaseKeyword : rest''') ->
                            return (Just . Pos pos . SwitchOperator expr $ var : vars, rest''')
                        (Just vars, Pos pos' t : rest''') -> do
                            tellError pos' $ "Expected the endcase keyword, found " ++ show t
                            return (Just . Pos pos . SwitchOperator expr $ var : vars, Pos pos' t : rest''')
                        (Just _, []) -> return (Nothing, [])
        (Just expr, Pos pos' t : rest') -> do
            tellError pos' $ "Expected the of keyword, found " ++ show t
            variant <- parseSwitchVariant $ Pos pos' t : rest'
            case variant of
                (Nothing, rest'') -> return (Nothing, rest'')
                (Just var, rest'') -> do
                    variants <- parseSwitchVariantsRest rest''
                    case variants of
                        (Nothing, rest''') -> return (Nothing, rest''')
                        (Just vars, Pos _ EndCaseKeyword : rest''') ->
                            return (Just . Pos pos . SwitchOperator expr $ var : vars, rest''')
                        (Just vars, Pos pos'' t' : rest''') -> do
                            tellError pos'' $ "Expected the endcase keyword, found " ++ show t'
                            return (Just . Pos pos . SwitchOperator expr $ var : vars, Pos pos'' t' : rest''')
                        (Just _, []) -> return (Nothing, [])
        (Just _, []) -> return (Nothing, [])
parseSwitchOperator (Pos pos t : rest) = do
    tellError pos $ "Expected the case keyword, found " ++ show t
    return (Nothing, rest)
parseSwitchOperator [] = return (Nothing, [])

parseSwitchVariant :: [Pos Token] -> Writer [String] (Maybe (Pos AST.Constant, Pos Operator), [Pos Token])
parseSwitchVariant tokens = do
    constant <- parseConstant tokens
    case constant of
        (Nothing, rest) -> return (Nothing, rest)
        (Just c, Pos _ Colon : rest) -> do
            operator <- parseOperator rest
            case operator of
                (Nothing, rest') -> return (Nothing, rest')
                (Just op, rest') -> return (Just (c, op), rest')
        (Just c, Pos pos t : rest) -> do
            tellError pos $ "Expected the colon (:) token, found " ++ show t
            operator <- parseOperator $ Pos pos t : rest
            case operator of
                (Nothing, rest') -> return (Nothing, rest')
                (Just op, rest') -> return (Just (c, op), rest')
        (Just _, []) -> return (Nothing, [])

parseSwitchVariantsRest :: [Pos Token] -> Writer [String] (Maybe [(Pos AST.Constant, Pos Operator)], [Pos Token])
parseSwitchVariantsRest (Pos _ EndCaseKeyword : rest) = return (Just [], rest)
parseSwitchVariantsRest (Pos _ Semicolon : rest) = do
    variant <- parseSwitchVariant rest
    case variant of
        (Nothing, rest') -> return (Nothing, rest')
        (Just var, rest') -> do
            variants <- parseSwitchVariantsRest rest'
            case variants of
                (Nothing, rest'') -> return (Nothing, rest'')
                (Just vars, rest'') -> return (Just $ var : vars, rest'')
parseSwitchVariantsRest (Pos pos t : rest) = do
    tellError pos $ "Expected either the endcase keyword or the semicolon (;), found: " ++ show t
    return (Just [], Pos pos t : rest)
parseSwitchVariantsRest [] = return (Nothing, [])
        

parseIfOperator :: [Pos Token] -> Writer [String] (Maybe (Pos Operator), [Pos Token])
parseIfOperator ((Pos pos IfKeyword) : rest) = do
    condition <- parseExpression rest
    case condition of
        (Nothing, rest') -> return (Nothing, rest')
        (Just expr, Pos _ ThenKeyword : rest') -> do
            operator1 <- parseOperator rest'
            case operator1 of
                n@(Nothing, _) -> return n
                (Just op1, Pos _ ElseKeyword : rest'') -> do
                    operator2 <- parseOperator rest''
                    case operator2 of
                        n@(Nothing, _) -> return n
                        (Just op2, rest''') -> return (Just . Pos pos $ IfOperator expr op1 op2, rest''')
                (Just op1, Pos pos' t : rest'') -> do
                    tellError pos' $ "Expected the else keyword, found " ++ show t
                    operator2 <- parseOperator rest''
                    case operator2 of
                        n@(Nothing, _) -> return n
                        (Just op2, rest''') -> return (Just . Pos pos $ IfOperator expr op1 op2, rest''')
                (Just _, []) -> return (Nothing, [])
        (Just expr, (Pos pos' t) : rest') -> do
            tellError pos' $ "Expected the then keyword, found " ++ show t
            operator1 <- parseOperator rest'
            case operator1 of
                n@(Nothing, _) -> return n
                (Just op1, Pos _ ElseKeyword : rest'') -> do
                    operator2 <- parseOperator rest''
                    case operator2 of
                        n@(Nothing, _) -> return n
                        (Just op2, rest''') -> return (Just . Pos pos $ IfOperator expr op1 op2, rest''')
                (Just op1, Pos pos'' t' : rest'') -> do
                    tellError pos'' $ "Expected the else keyword, found " ++ show t'
                    operator2 <- parseOperator rest''
                    case operator2 of
                        n@(Nothing, _) -> return n
                        (Just op2, rest''') -> return (Just . Pos pos $ IfOperator expr op1 op2, rest''')
                (Just _, []) -> return (Nothing, [])
        (Just _, []) -> return (Nothing, [])
parseIfOperator ((Pos pos t) : rest) = do
    tellError pos $ "Expected the if keyword, found " ++ show t
    return (Nothing, Pos pos t : rest)
parseIfOperator [] = return (Nothing, [])

parseExpression :: [Pos Token] -> Writer [String] (Maybe (Pos Expression), [Pos Token])
parseExpression tokens = do
    parsed <- parseSumExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just (Pos pos expr), Pos opPos t : ts) | isJust $ toRelationOp t -> do
            parsed' <- parseSumExpression ts
            return $ case parsed' of
                (Nothing, rest) -> (Nothing, rest)
                (Just expr', rest) ->
                    (fmap (Pos pos) $ pure RelationExpression <*> (Pos opPos <$> toRelationOp t) <*> pure (Pos pos expr) <*> pure expr', rest)
        (Just (Pos pos expr), ts) -> return (Just . Pos pos . SimpleExpression $ Pos pos expr, ts)

parseSumExpression :: [Pos Token] -> Writer [String] (Maybe (Pos SumExpression), [Pos Token])
parseSumExpression tokens = do
    parsed <- parseProductExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just (Pos pos prod), rest) -> parseSumLoop (Pos pos . SimpleSumExpression $ Pos pos prod) rest
    where
        parseSumLoop (Pos accPos acc) (Pos opPos t : ts) | isJust $ toAdditionOp t = do
            parsed <- parseProductExpression ts
            case parsed of
                (Nothing, rest) -> return (Nothing, rest)
                (Just expr, rest) -> let
                    newAcc = Pos accPos $ AdditionExpression (Pos opPos . fromJust $ toAdditionOp t) (Pos accPos acc) expr
                    in parseSumLoop newAcc rest
        parseSumLoop acc tokens' = return (Just acc, tokens')
        
parseProductExpression :: [Pos Token] -> Writer [String] (Maybe (Pos ProductExpression), [Pos Token])
parseProductExpression tokens = do
    parsed <- parseBasicExpression tokens
    case parsed of
        (Nothing, rest) -> return (Nothing, rest)
        (Just (Pos pos basic), rest) -> parseProductLoop (Pos pos . SimpleProductExpression $ Pos pos basic) rest
    where
        parseProductLoop (Pos accPos acc) (Pos opPos t : ts) | isJust $ toMultiplicationOp t = do
            parsed <- parseBasicExpression ts
            case parsed of
                (Nothing, rest) -> return (Nothing, rest)
                (Just expr, rest) -> let
                    newAcc = Pos accPos $ MultiplicationExpression (Pos opPos . fromJust $ toMultiplicationOp t) (Pos accPos acc) expr
                    in parseProductLoop newAcc rest
        parseProductLoop acc tokens' = return (Just acc, tokens')

parseBasicExpression :: [Pos Token] -> Writer [String] (Maybe (Pos BasicExpression), [Pos Token])
parseBasicExpression [] = return (Nothing, [])
parseBasicExpression (Pos pos token : rest) = case token of
    Identifier name -> return (Just . Pos pos . NameExpression . Pos pos $ Name name, rest)
    IntegerConstant i -> return (Just . Pos pos . ConstantExpression . Pos pos $ AST.IntegerConstant i, rest)
    BooleanConstant b -> return (Just . Pos pos . ConstantExpression . Pos pos $ AST.BooleanConstant b, rest)
    NotOperator -> do
        parsed <- parseBasicExpression rest
        return $ case parsed of
            (Nothing, rest') -> (Nothing, rest')
            (Just basic, rest') -> (Just . Pos pos $ NotExpression basic, rest')
    OpenParen -> do
        parsed <- parseExpression rest
        return $ case parsed of
            (Just expr, (Pos _ CloseParen : rest')) -> (Just . Pos pos $ ParenthesizedExpression expr, rest')
            (_, rest') -> (Nothing, rest')
    _ -> return (Nothing, rest)

parseConstant :: [Pos Token] -> Writer [String] (Maybe (Pos AST.Constant), [Pos Token])
parseConstant (Pos pos (IntegerConstant i) : rest) = return (Just . Pos pos $ AST.IntegerConstant i, rest)
parseConstant (Pos pos (BooleanConstant b) : rest) = return (Just . Pos pos $ AST.BooleanConstant b, rest)
parseConstant (Pos pos t : rest) = do
    tellError pos $ "Expected a constant, found: " ++ show t
    return (Nothing, Pos pos t : rest)
parseConstant [] = return (Nothing, [])

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
