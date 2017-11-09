module TigerTypes where

data Atom = Atom String
data Comment = Comment String
type DeclarationList = [Declaration]
data Declaration =
    TypeDec Atom Type
    | VarDec Atom (Maybe Atom) Expression
    | FnDec Atom TypeFields (Maybe Atom) Expression
type TypeFields = [(Atom, Atom)]
data Type =
    TypeId Atom
    | RecordOf TypeFields
    | ArrayOf Atom
data LValue =
    Id Atom
    | RecordAccess LValue Atom
    | ArraySubscript LValue Expression
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
