module Builtins where

import Data.Maybe (catMaybes)
import qualified Environment as Env
import qualified Symbol as Sym
import Types ( ProgramType(..)
              , TypeEnv(..)
              )

initialTypeEnv :: TypeEnv
initialTypeEnv =
    let table = initialSymbolTable
    in TypeEnv { sym = table
               , tEnv = initialTypeTypes table
               , vEnv = initialValueTypes table
               }

initialSymbolTable :: Sym.SymbolTable
initialSymbolTable =
    let
      names = [ "int"
              , "string"
              , "print"
              , "flush"
              , "getchar"
              , "ord"
              , "chr"
              , "size"
              , "substring"
              , "concat"
              , "not"
              , "exit"
              ]
      empty = Sym.newTable 0
    in
      foldr (\n tbl -> snd $ Sym.put n tbl) empty names

initialTypeTypes :: Sym.SymbolTable -> Env.Environment ProgramType
initialTypeTypes t =
    let
      types = [ ("int", TigerInt)
              , ("string", TigerStr)
              ]
    in
      Env.fromList $ catMaybes (symbolizeTypePair t <$> types)

initialValueTypes :: Sym.SymbolTable -> Env.Environment ProgramType
initialValueTypes t =
    let
      types = [ ("print", Function [TigerStr] Unit)
              , ("flush", Function [] Unit)
              , ("getchar", Function [] TigerStr)
              , ("ord", Function [TigerStr] TigerInt)
              , ("chr", Function [TigerInt] TigerStr)
              , ("size", Function [TigerStr] TigerInt)
              , ("substring", Function [TigerStr, TigerInt, TigerInt] TigerStr)
              , ("concat", Function [TigerStr, TigerStr] TigerStr)
              , ("not", Function [TigerInt] TigerInt)
              , ("exit", Function [TigerInt] Unit)
              ]
    in
      Env.fromList $ catMaybes (symbolizeTypePair t <$> types)


symbolizeTypePair :: Sym.SymbolTable ->
                     (String, ProgramType) ->
                     Maybe (Sym.Symbol, ProgramType)
symbolizeTypePair t (n, typ) = (,) <$> Sym.get n t <*> (Just typ)
