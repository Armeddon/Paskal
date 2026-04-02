module AST where

import Util (Pos)

data AST = AST (Pos Name) (Pos VariableSection) (Pos OperatorSection)
newtype Name = Name String deriving Eq
data Type = IntegerType | BooleanType deriving Eq
newtype VariableSection = VariableSection [([Pos Name], Pos Type)]
newtype OperatorSection = OperatorSection [Pos Operator]
data Operator = AssignmentOperator (Pos Name) (Pos Expression)
              | OutputOperator [Pos Expression]
              | CompoundOperator [Pos Operator]
              | WhileLoopOperator (Pos Expression) (Pos Operator)
              | SwitchOperator (Pos Expression) [(Pos Constant, Pos Operator)]
              | IfOperator (Pos Expression) (Pos Operator) (Pos Operator)
data Expression = SimpleExpression (Pos SumExpression)
                | RelationExpression (Pos RelationOperation) (Pos SumExpression) (Pos SumExpression)
data RelationOperation = Greater | Less | NotEqual | LessOrEqual | GreaterOrEqual | Equal
data SumExpression = SimpleSumExpression (Pos ProductExpression)
                   | AdditionExpression (Pos AdditionOperation) (Pos SumExpression) (Pos ProductExpression)
data AdditionOperation = Add | Subtract | Or
data ProductExpression = SimpleProductExpression (Pos BasicExpression)
                       | MultiplicationExpression (Pos MultiplicationOperation) (Pos ProductExpression) (Pos BasicExpression)
data BasicExpression = NameExpression (Pos Name)
                     | ConstantExpression (Pos Constant)
                     | ParenthesizedExpression (Pos Expression)
                     | NotExpression (Pos BasicExpression)
data MultiplicationOperation = Multiply | Divide | Modulus | And
data Constant = IntegerConstant Int | BooleanConstant Bool

typeOfConstant :: Constant -> Type
typeOfConstant (IntegerConstant _) = IntegerType
typeOfConstant (BooleanConstant _) = BooleanType
