module AST where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)
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

instance Ord Operator where
  compare x y = compare (precedence x) (precedence y)
    where
      precedence op = fromMaybe 0 (
        findIndex id $
          elem op <$> [ [Or]
                      , [And]
                      , [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual]
                      , [Addition, Subtraction]
                      , [Multiplication, Division]
                      ])


position :: Expression -> SourcePos
position (LValExp pos _) = pos
position (ValuelessExpression pos _) = pos
position (Nil pos) = pos
position (Sequence pos _) = pos
position (NoValue pos) = pos
position (IntLiteral pos _) = pos
position (StringLiteral pos _) = pos
position (Negation pos _) = pos
position (FunctionCall pos _ _) = pos
position (BinOp pos _ _ _) = pos
position (RecordCreation pos _ _) = pos
position (ArrayCreation pos _ _ _) = pos
position (Assignment pos _ _) = pos
position (IfThenElse pos _ _ _) = pos
position (IfThen pos _ _) = pos
position (While pos _ _) = pos
position (For pos _ _ _ _) = pos
position (Break pos) = pos
position (Let pos _ _) = pos
position (Grouped pos _) = pos
