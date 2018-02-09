module Symbol where

import qualified Data.Map.Strict as Map

data Symbol = Symbol String Integer

instance Show Symbol where
    show (Symbol name _) = name

instance Eq Symbol where
    (==) (Symbol _ i_1) (Symbol _ i_2) = i_1 == i_2

instance Ord Symbol where
    compare (Symbol _ i_1) (Symbol _ i_2) = compare i_1 i_2

data SymbolTable = SymbolTable ((Map.Map String Symbol), Integer)
                 deriving (Show, Eq)

newTable :: Integer -> SymbolTable
newTable i = SymbolTable (Map.empty, i)

put :: String -> SymbolTable -> (Symbol, SymbolTable)
put s tbl@(SymbolTable (m, i)) =
    case Map.lookup s m of
      Just sym -> (sym, tbl)
      Nothing -> ( Symbol s i
                 , SymbolTable ( Map.insert s (Symbol s i) m
                               , i + 1
                               )
                 )

get :: String -> SymbolTable -> Maybe Symbol
get s (SymbolTable (m, _)) = Map.lookup s m
