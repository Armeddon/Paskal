module Lexer (tokenize) where
import Data.Char (isDigit, isAlphaNum, isSpace)
import Data.Either (partitionEithers)
import Data.List.Extra (wordsBy)
import Safe (lastMay)
import Token
import Util

tokenize :: Filename -> String -> Either [Pos String] [Pos Token]
tokenize file input = let
    enumerated = enumerateSymbols file input
    eofPosition = maybe (file, 0, 0) (\(Pos p _) -> p) $ lastMay enumerated
    withSurroundedSymbols = surroundSymbols enumerated
    wordSplit = wordsBy (isSpace . \(Pos _ c) -> c) withSurroundedSymbols
    (lefts, rights) = partitionEithers $ map (posBoth . tokenizeWord) wordSplit
    in if not $ null lefts then Left lefts else Right $ rights ++ [(Pos eofPosition EOF)]

tokenizeWord :: [Pos Char] -> Pos (Either String Token)
tokenizeWord input@((Pos pos _):_) = Pos pos $ case map (\(Pos _ c) -> c) input of
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
    "="       -> Right EqualOperator
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
tokenizeWord [] = Pos (Filename "", 0, 0) $ Left ""

enumerateSymbols :: Filename -> String -> [Pos Char]
enumerateSymbols f = tail . scanl (\(Pos (_, r, c) _) chr ->
        if chr == '\n'
        then Pos (f, r+1, 1) chr
        else Pos (f, r, c+1) chr
    ) (Pos (f, 1, 0) '_')

surroundSymbols :: [Pos Char] -> [Pos Char]
surroundSymbols text = foldr surroundSymbol text ".,();"

surroundSymbol :: Char -> [Pos Char] -> [Pos Char]
surroundSymbol c1 ((Pos pos c2):rest) | c1 == c2  = [
        Pos (Filename "", 0, 0) ' ',
        Pos pos c1,
        Pos (Filename "", 0, 0) ' '
    ] ++ surroundSymbol c1 rest
                                   | otherwise = Pos pos c2 : surroundSymbol c1 rest
surroundSymbol _ [] = []

posBoth :: Pos (Either a b) -> Either (Pos a) (Pos b)
posBoth (Pos pos (Left x)) = Left $ Pos pos x
posBoth (Pos pos (Right x)) = Right $ Pos pos x
