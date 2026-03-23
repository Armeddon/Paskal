module Lexer (tokenize) where
import Data.Char (isDigit, isAlphaNum, isSpace)
import Data.Either (partitionEithers)
import Data.List.Extra (wordsBy)
import Safe (lastMay)
import Token
import Util

tokenize :: String -> Either [Pos String] [Pos Token]
tokenize input = let
    enumerated = enumerateSymbols input
    eofPosition = maybe (0, 0) fst $ lastMay enumerated
    withSurroundedSymbols = surroundSymbols enumerated
    wordSplit = wordsBy (isSpace . snd) withSurroundedSymbols
    (lefts, rights) = partitionEithers $ map (posBoth . tokenizeWord) wordSplit
    in if not $ null lefts then Left lefts else Right $ rights ++ [(eofPosition, EOF)]

tokenizeWord :: [Pos Char] -> Pos (Either String Token)
tokenizeWord input@((pos, _):_) = (pos, case map snd input of
    "program" -> Right ProgramKeyword
    "var"     -> Right VarKeyword
    "begin"   -> Right BeginKeyword
    "end"     -> Right EndKeyword
    "writeln" -> Right WritelnKeyword
    "while"   -> Right WhileKeyword
    "do"      -> Right DoKeyword
    "case"    -> Right CaseKeyword
    "of"      -> Right OfKeyword
    "endcase" -> Right EndCaseKeyword
    "if"      -> Right IfKeyword
    "then"    -> Right ThenKeyword
    "else"    -> Right ElseKeyword
    "integer" -> Right IntegerType
    "boolean" -> Right BooleanType
    ":="      -> Right AssignmentOperator
    ">"       -> Right GreaterOperator
    "<"       -> Right LessOperator
    "<>"      -> Right NotEqualOperator
    "<="      -> Right LessOrEqualOperator
    ">="      -> Right GreaterOrEqualOperator
    "="       -> Right GreaterOrEqualOperator
    "+"       -> Right AddOperator
    "-"       -> Right SubtractOperator
    "or"      -> Right OrOperator
    "*"       -> Right MultiplyOperator
    "div"     -> Right DivideOperator
    "mod"     -> Right ModulusOperator
    "and"     -> Right AndOperator
    "not"     -> Right NotOperator
    ";"       -> Right Semicolon
    ","       -> Right Comma
    "."       -> Right Dot
    ":"       -> Right Colon
    "("       -> Right OpenParen
    ")"       -> Right CloseParen
    "true"    -> Right $ BooleanConstant True
    "false"   -> Right $ BooleanConstant False
    string | all isDigit string -> Right . IntegerConstant $ read string
           | all (\c -> isAlphaNum c || c == '_') string -> Right $ Identifier string
           | otherwise -> Left string
    )
tokenizeWord [] = ((0, 0), Left "")

enumerateSymbols :: String -> [Pos Char]
enumerateSymbols = tail . scanl (\((r, c), _) chr -> if chr == '\n' then ((r+1, 1), chr) else ((r, c+1), chr)) ((1, 0), '_')

surroundSymbols :: [Pos Char] -> [Pos Char]
surroundSymbols text = foldr surroundSymbol text ".,();"

surroundSymbol :: Char -> [Pos Char] -> [Pos Char]
surroundSymbol c1 ((pos, c2):rest) | c1 == c2  = [((0, 0), ' '), (pos, c1), ((0, 0), ' ')] ++ surroundSymbol c1 rest
                                   | otherwise = (pos, c2) : surroundSymbol c1 rest
surroundSymbol _ [] = []

posBoth :: Pos (Either a b) -> Either (Pos a) (Pos b)
posBoth (pos, Left x) = Left (pos, x)
posBoth (pos, Right x) = Right (pos, x)
