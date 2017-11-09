module TigerTypes where

type Atom = String
type TypeName = String
data Comment = Comment String
  deriving (Show, Eq)
type DeclarationList = [Declaration]
data Declaration =
    TypeDec TypeName Type
    | VarDec Atom (Maybe TypeName) Expression
    | FnDec Atom TypeFields (Maybe TypeName) Expression
    deriving (Show, Eq)
type TypeFields = [(Atom, TypeName)]
data Type =
    TypeId TypeName
    | RecordOf TypeFields
    | ArrayOf TypeName
    deriving (Show, Eq)
data LValue =
    Id Atom
    | RecordAccess LValue Atom
    | ArraySubscript LValue Expression
    deriving (Show, Eq)
data Expression =
    LValExp LValue
    | ValuelessExpression Expression
    | Nil
    | Sequence [Expression]
    | NoValue Expression
    | IntLiteral Integer
    | StringLiteral String
    | Negation Expression
    | FunctionCall Atom [Expression]
    | BinOp Expression Operator Expression
    | Record
    | Array
    | Assignment LValue Expression
    | IfThenElse Expression Expression Expression
    | IfThen Expression Expression
    | While Expression Expression
    | For Atom Expression Expression Expression
    | Break
    | Let DeclarationList [Expression]
    | Grouped [Expression]
    deriving (Show, Eq)
data Operator =
    Addition
    | Subtraction
    | Multiplication
    | Division
    | Equality
    | NonEquality
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual
    | And
    | Or
    deriving (Show, Eq)
