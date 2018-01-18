module TigerTypes where

import Text.Parsec.Pos (SourcePos)

type Atom = String
type TypeName = String
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
    LValExp SourcePos LValue
    | DecExp SourcePos Declaration
    | ValuelessExpression SourcePos Expression
    | Nil SourcePos
    | Sequence SourcePos [Expression]
    | NoValue SourcePos
    | IntLiteral SourcePos Integer
    | StringLiteral SourcePos String
    | Negation SourcePos Expression
    | FunctionCall SourcePos Atom [Expression]
    | BinOp SourcePos Operator Expression Expression
    | RecordCreation SourcePos TypeName [(Atom, Expression)]
    | ArrayCreation SourcePos TypeName Expression Expression
    | Assignment SourcePos LValue Expression
    | IfThenElse SourcePos Expression Expression Expression
    | IfThen SourcePos Expression Expression
    | While SourcePos Expression Expression
    | For SourcePos Atom Expression Expression Expression
    | Break SourcePos
    | Let SourcePos DeclarationList [Expression]
    | Grouped SourcePos Expression
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
