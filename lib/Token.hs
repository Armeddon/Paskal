module Token where

data Token = ProgramKeyword
           | VarKeyword
           | BeginKeyword
           | EndKeyword
           | WritelnKeyword
           | WhileKeyword
           | DoKeyword
           | CaseKeyword
           | OfKeyword
           | EndCaseKeyword
           | IfKeyword
           | ThenKeyword
           | ElseKeyword
           | IntegerType
           | BooleanType
           | IntegerConstant Int
           | BooleanConstant Bool
           | Identifier String
           | AssignmentOperator
           | GreaterOperator
           | LessOperator
           | NotEqualOperator
           | LessOrEqualOperator
           | GreaterOrEqualOperator
           | EqualOperator
           | AddOperator
           | SubtractOperator
           | OrOperator
           | MultiplyOperator
           | DivideOperator
           | ModulusOperator
           | AndOperator
           | NotOperator
           | Comma
           | Colon
           | Semicolon
           | Dot
           | OpenParen
           | CloseParen
           | EOF

instance Show Token where
    show ProgramKeyword = "program keyword"
    show VarKeyword = "var keyword"
    show BeginKeyword = "begin keyword"
    show EndKeyword = "end keyword"
    show WritelnKeyword = "writeln function"
    show WhileKeyword = "while keyword"
    show DoKeyword = "do keyword"
    show CaseKeyword = "case keyword"
    show OfKeyword = "of keyword"
    show EndCaseKeyword = "endcase keyword"
    show IfKeyword = "if keyword"
    show ThenKeyword = "then keyword"
    show ElseKeyword = "else keyword"
    show IntegerType = "type (integer)"
    show BooleanType = "type (boolean)"
    show (IntegerConstant n) = "integer constant (" ++ show n ++ ")"
    show (BooleanConstant b) = "boolean constant (" ++ if b then "true" else "false" ++ ")"
    show (Identifier name) = "identifier (" ++ name ++ ")"
    show AssignmentOperator = "assignment operator (=)"
    show GreaterOperator = "greater operator (>)"
    show LessOperator = "less operator (<)"
    show NotEqualOperator = "not equals operator (<>)"
    show LessOrEqualOperator = "less or equals operator (<=)"
    show GreaterOrEqualOperator = "greater or equals operator (>=)"
    show EqualOperator = "equality operator (=)"
    show AddOperator = "addition operator (+)"
    show SubtractOperator = "subtraction operator (-)"
    show OrOperator = "disjunction operator (or)"
    show MultiplyOperator = "multiplication operator (*)"
    show DivideOperator = "division operator (div)"
    show ModulusOperator = "modulus operator (mod)"
    show AndOperator = "conjunction operator (and)"
    show NotOperator = "negation operator (not)"
    show Colon = "colon (:)"
    show Comma = "comma (,)"
    show Semicolon = "semicolon (;)"
    show Dot = "dot (.)"
    show OpenParen = "opening parenthesis ('(')"
    show CloseParen = "closing parentesis (')')"
    show EOF = "end of file"
