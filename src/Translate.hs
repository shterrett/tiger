module Translate where

import qualified Symbol as Sym
import qualified Temporary as Tmp
import qualified Frame

data Frame.Frame a => Level a =
    Outermost
    | Nested (Level a) a
    deriving (Show, Eq)

data Frame.Frame a => Access a =
    Access (Level a) Frame.Access
    deriving (Show, Eq)

newLevel :: Level Frame.X86Frame ->
            Tmp.Label ->
            [Frame.Var] ->
            Sym.SymbolTable ->
            (Sym.SymbolTable, Level Frame.X86Frame)
newLevel l n fs tbl =
    let frame = Frame.newX86Frame n (Frame.Escape:fs) tbl
    in (Nested l) <$> frame

formals :: Frame.Frame a => Level a -> [Access a]
formals Outermost = []
formals l@(Nested _ frame) = (Access l) <$>  drop 1 (Frame.formals frame)

allocLocal :: Frame.Frame a =>
              Frame.Var ->
              Level a ->
              Sym.SymbolTable ->
              ((Sym.SymbolTable, Level a), Access a)
allocLocal v Outermost tbl = undefined
allocLocal v (Nested parent frame) tbl =
    let
      ((tbl', frame'), local) = Frame.allocLocal v frame tbl
      level = Nested parent frame'
    in ((tbl', level), Access level local)

locals :: Frame.Frame a => Level a -> [Access a]
locals Outermost = []
locals l@(Nested _ frame) = (Access l) <$> (Frame.locals frame)
