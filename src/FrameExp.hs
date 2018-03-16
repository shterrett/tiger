module FrameExp where

import Text.Parsec.Pos (SourcePos)
import qualified Symbol as Sym
import qualified Environment as Env
import qualified Frame
import AST ( Atom
           , TypeName
           , TypeFields
           , Type
           , Operator
           )

data Frame.Frame a => Level a =
    Outermost
    | Nested (Level a) a
    deriving (Show, Eq)

data Frame.Frame a => Access a =
    Access (Level a) Frame.Access
    deriving (Show, Eq)

data Frame.Frame a => VarEnv a =
    VarEnv { vEnv :: Env.Environment (Access a)
           , sym :: Sym.SymbolTable
           }
           deriving (Show, Eq)

data Frame.Frame a => LEnv a = LEnv (Frame.NewFrame a) (VarEnv a) (Level a)
                   deriving (Show, Eq)

data Frame.Frame a => Declaration a =
    TypeDec TypeName Type
    | VarDec Atom (FExp a)
    | FnDec Atom TypeFields (FExp a)
    deriving (Show, Eq)
data Frame.Frame a => LValue a =
    Id Atom
    | RecordAccess (LValue a) Atom
    | ArraySubscript (LValue a) (FExp a)
    deriving (Show, Eq)
data Frame.Frame a => FExp a =
    LValExp SourcePos (LEnv a) (LValue a)
    | DecExp SourcePos (LEnv a) (Declaration a)
    | ValuelessExpression SourcePos (LEnv a) (FExp a)
    | Nil SourcePos (LEnv a)
    | Sequence SourcePos (LEnv a) [(FExp a)]
    | NoValue SourcePos (LEnv a)
    | IntLiteral SourcePos (LEnv a) Integer
    | StringLiteral SourcePos (LEnv a) String
    | Negation SourcePos (LEnv a) (FExp a)
    | FunctionCall SourcePos (LEnv a) Atom [(FExp a)]
    | BinOp SourcePos (LEnv a) Operator (FExp a) (FExp a)
    | RecordCreation SourcePos (LEnv a) TypeName [(Atom, (FExp a))]
    | ArrayCreation SourcePos (LEnv a) TypeName (FExp a) (FExp a)
    | Assignment SourcePos (LEnv a) (LValue a) (FExp a)
    | IfThenElse SourcePos (LEnv a) (FExp a) (FExp a) (FExp a)
    | IfThen SourcePos (LEnv a) (FExp a) (FExp a)
    | While SourcePos (LEnv a) (FExp a) (FExp a)
    | For SourcePos (LEnv a) Atom (FExp a) (FExp a) (FExp a)
    | Break SourcePos (LEnv a)
    | Let SourcePos (LEnv a) [Declaration a] [(FExp a)]
    | Grouped SourcePos (LEnv a) (FExp a)
    deriving (Show, Eq)
