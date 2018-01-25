module Semantics where

import qualified Environment as Env
import qualified Symbol as Sym
import Typing (TypeEnv, ProgramType)
import Translation (ValEnv, IL)

data TotalEnv = TotalEnv { tEnv :: TypeEnv
                         , vEnv :: ValEnv
                         , sym :: Sym.SymbolTable
                         }

type ExpTy = (IL, ProgramType)
