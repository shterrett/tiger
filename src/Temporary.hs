module Temporary where

import Data.Maybe (fromMaybe)
import qualified Symbol as Sym

data Var = Var Sym.Symbol

instance Show Var where
    show (Var s) = show s

instance Eq Var where
    (==) (Var s1) (Var s2) = s1 == s2

data Label = Label Sym.Symbol

instance Show Label where
    show (Label s) = show s

instance Eq Label where
    (==) (Label s1) (Label s2) = s1 == s2

newVar :: Sym.SymbolTable -> (Var, Sym.SymbolTable)
newVar tbl =
    let (sym, tbl') = Sym.putUnique "tmp_v" tbl
    in (Var sym, tbl')

newLabel :: Sym.SymbolTable -> (Label, Sym.SymbolTable)
newLabel tbl =
    let (sym, tbl') = Sym.putUnique "tmp_l" tbl
    in (Label sym, tbl')
