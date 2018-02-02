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
put s (SymbolTable (m, i)) = let sym = Symbol s i
                             in ( sym
                                , SymbolTable ( Map.insertWith (flip const) s sym m
                                              , i + 1
                                              )
                                )

get :: String -> SymbolTable -> Maybe Symbol
get s (SymbolTable (m, _)) = Map.lookup s m
