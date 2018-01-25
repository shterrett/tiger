module Semantics where

import qualified Environment as Env
import qualified Symbol as Sym
import Typing (ProgramType)
import Translation (IL)

data TotalEnv = TotalEnv { tEnv :: Env.TypeEnv
                         , vEnv :: Env.ValEnv
                         , sym :: Sym.SymbolTable
                         }

type ExpTy = (IL, ProgramType)
