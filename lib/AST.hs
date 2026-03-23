module AST where

data AST = AST Name VariableSection OperatorSection
newtype Name = Name String deriving Eq
data Type = IntegerType | BooleanType deriving Eq
newtype VariableSection = VariableSection [([Name], Type)]
newtype OperatorSection = OperatorSection [Operator]
data Operator = AssignmentOperator Name Expression
              | OutputOperator [Expression]
              | CompoundOperator [Operator]
              | WhileLoopOperator Expression Operator
              | SwitchOperator Expression [(Constant, Operator)]
              | IfOperator Expression Operator Operator
data Expression = SimpleExpression SumExpression
                | RelationExpression RelationOperation SumExpression SumExpression
data RelationOperation = Greater | Less | NotEqual | LessOrEqual | GreaterOrEqual | Equal
data SumExpression = SimpleSumExpression ProductExpression
                   | AdditionExpression AdditionOperation SumExpression ProductExpression
data AdditionOperation = Add | Subtract | Or
data ProductExpression = SimpleProductExpression BasicExpression
                       | MultiplicationExpression MultiplicationOperation ProductExpression BasicExpression
data BasicExpression = NameExpression Name
                     | ConstantExpression Constant
                     | ParenthesizedExpression Expression
                     | NotExpression BasicExpression
data MultiplicationOperation = Multiply | Divide | Modulus | And
data Constant = IntegerConstant Int | BooleanConstant Bool

typeOf :: Constant -> Type
typeOf (IntegerConstant _) = IntegerType
typeOf (BooleanConstant _) = BooleanType
