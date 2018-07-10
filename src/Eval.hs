module Eval where

import Types ( TypeInfo
             , TypeError
             , TExp(..)
             , getType
             , getTEnv
             , typeInfo
             , Declarable(..)
             , isNullable
             )
import AST (Atom)
import qualified Types as T
import qualified Symbol as Sym
import qualified Environment as Env

data Result =
    TigerInt Integer
    | TigerStr String
    | Record [(Atom, Result)]
    | Array [Result]
    | Unit
    deriving (Eq)

instance Show Result where
    show (TigerInt i) = show i
    show (TigerStr s) = show s
    show (Record fs) = show fs
    show (Array rs) = show rs
    show Unit = "()"

type EvalError = String

data EvalEnv = EvalEnv { vEnv :: Env.Environment Result
                       , sym :: Sym.SymbolTable
                       }

eval :: EvalEnv -> TExp -> Either EvalError Result
eval env (IntLiteral _ _ i)  = Right $ TigerInt i
eval env (StringLiteral _ _ s) = Right $ TigerStr s
