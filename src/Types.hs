module Types where

import Data.List (intersect)
import Text.Parsec.Pos (SourcePos)
import qualified Symbol as Sym
import qualified Environment as Env
import AST ( Atom
           , TypeName
           , Operator
           )

data Declaration =
    VarDec Atom TExp
    | FnDec Atom [Atom] TExp
    | TypeDec
    deriving (Show, Eq)
data LValue =
    Id Atom
    | RecordAccess LValue Atom
    | ArraySubscript LValue TExp
    deriving (Show, Eq)

data TExp =
    LValExp SourcePos TypeInfo LValue
    | DecExp SourcePos TypeInfo Declaration
    | ValuelessExpression SourcePos TypeInfo TExp
    | Nil SourcePos TypeInfo
    | Sequence SourcePos TypeInfo [TExp]
    | NoValue SourcePos TypeInfo
    | IntLiteral SourcePos TypeInfo Integer
    | StringLiteral SourcePos TypeInfo String
    | Negation SourcePos TypeInfo TExp
    | FunctionCall SourcePos TypeInfo Atom [TExp]
    | BinOp SourcePos TypeInfo Operator TExp TExp
    | RecordCreation SourcePos TypeInfo TypeName [(Atom, TExp)]
    | ArrayCreation SourcePos TypeInfo TypeName TExp TExp
    | Assignment SourcePos TypeInfo LValue TExp
    | IfThenElse SourcePos TypeInfo TExp TExp TExp
    | IfThen SourcePos TypeInfo TExp TExp
    | While SourcePos TypeInfo TExp TExp
    | For SourcePos TypeInfo Atom TExp TExp TExp
    | Break SourcePos TypeInfo
    | Let SourcePos TypeInfo [Declaration] [TExp]
    | Grouped SourcePos TypeInfo TExp
    deriving (Show, Eq)

data ProgramType =
    TigerInt
    | TigerStr
    | Record [(Atom, ProgramType)]
    | Array ProgramType
    | Unit
    | Name Sym.Symbol (Maybe ProgramType)
    | Function [ProgramType] ProgramType

instance Show ProgramType where
    show TigerInt = "Integer"
    show TigerStr = "String"
    show (Record fields) = "{" ++ show fields ++ "}"
    show (Array typ) = "[" ++ show typ ++ "]"
    show Unit = "()"
    show (Name name _) = show name
    show (Function args return) = "fn" ++
                                  "(" ++ show args ++ ")" ++
                                  " : " ++ show return

instance Eq ProgramType where
    (==) (TigerInt) (TigerInt) = True
    (==) (TigerStr) (TigerStr) = True
    (==) Unit Unit = True
    (==) n@(Name _ _) m@(Name _ _) = cmpAliasTypes n m
    (==) (Name _ (Just t1)) t2 = t1 == t2
    (==) t1 (Name _ (Just t2)) = t1 == t2
    (==) _ _ = False

cmpAliasTypes :: ProgramType -> ProgramType -> Bool
cmpAliasTypes m@(Name _ t1) n@(Name _ t2) =
    if intersect (aliasChain m) (aliasChain n) == []
      then t1 == t2
      else True

aliasChain :: ProgramType -> [Sym.Symbol]
aliasChain (Name n (Just t)) = n:(aliasChain t)
aliasChain (Name n Nothing) = [n]
aliasChain _ = []

isNullable :: ProgramType -> Bool
isNullable TigerInt = False
isNullable TigerStr = False
isNullable (Record _) = True
isNullable (Array _) = False
isNullable Unit = False
isNullable (Name _ (Just t)) = isNullable t
isNullable (Name _ Nothing) = True
isNullable (Function _ _) = False

data TypeEnv = TypeEnv { tEnv :: Env.Environment ProgramType
                       , vEnv :: Env.Environment ProgramType
                       , sym :: Sym.SymbolTable
                       }
  deriving(Show, Eq)

type TypeError = String

type TypeInfo = (TypeEnv, ProgramType)

data Declarable = Field
               | Identifier
               | Type
               | Fn
instance Show Declarable where
    show Field = "field"
    show Identifier = "identifier"
    show Type = "type"
    show Fn = "function"

position :: TExp -> SourcePos
position (LValExp pos _ _) = pos
position (DecExp pos _ _) = pos
position (ValuelessExpression pos _ _) = pos
position (Nil pos _) = pos
position (Sequence pos _ _) = pos
position (NoValue pos _) = pos
position (IntLiteral pos _ _) = pos
position (StringLiteral pos _ _) = pos
position (Negation pos _ _) = pos
position (FunctionCall pos _ _ _) = pos
position (BinOp pos _ _ _ _) = pos
position (RecordCreation pos _ _ _) = pos
position (ArrayCreation pos _ _ _ _) = pos
position (Assignment pos _ _ _) = pos
position (IfThenElse pos _ _ _ _) = pos
position (IfThen pos _ _ _) = pos
position (While pos _ _ _) = pos
position (For pos _ _ _ _ _) = pos
position (Break pos _) = pos
position (Let pos _ _ _) = pos
position (Grouped pos _ _) = pos

typeInfo :: TExp -> TypeInfo
typeInfo (LValExp _ ti _) = ti
typeInfo (DecExp _ ti _) = ti
typeInfo (ValuelessExpression _ ti _) = ti
typeInfo (Nil _ ti) = ti
typeInfo (Sequence _ ti _) = ti
typeInfo (NoValue _ ti) = ti
typeInfo (IntLiteral _ ti _) = ti
typeInfo (StringLiteral _ ti _) = ti
typeInfo (Negation _ ti _) = ti
typeInfo (FunctionCall _ ti _ _) = ti
typeInfo (BinOp _ ti _ _ _) = ti
typeInfo (RecordCreation _ ti _ _) = ti
typeInfo (ArrayCreation _ ti _ _ _) = ti
typeInfo (Assignment _ ti _ _) = ti
typeInfo (IfThenElse _ ti _ _ _) = ti
typeInfo (IfThen _ ti _ _) = ti
typeInfo (While _ ti _ _) = ti
typeInfo (For _ ti _ _ _ _) = ti
typeInfo (Break _ ti) = ti
typeInfo (Let _ ti _ _) = ti
typeInfo (Grouped _ ti _) = ti

getType :: TExp -> ProgramType
getType = snd . typeInfo

getTEnv :: TExp -> TypeEnv
getTEnv = fst . typeInfo
