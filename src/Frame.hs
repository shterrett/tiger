module Frame where

import qualified Symbol as Sym
import qualified Temporary as Tmp

data Access =
    InFrame Int
    | InReg Tmp.Var
    deriving (Eq, Show)

data Var =
    Escape
    | NoEscape
    deriving (Eq, Show)

class Frame a where
    name :: a -> Tmp.Label
    formals :: a -> [Access]
    locals :: a -> [Access]
    allocLocal :: Var -> a -> Sym.SymbolTable -> ((Sym.SymbolTable, a), Access)

data NewFrame a = NewFrame {
                  getFn :: Tmp.Label -> [Var] -> Sym.SymbolTable -> (Sym.SymbolTable, a)
                }
instance Frame a => Show (NewFrame a) where
  show _ = "NewFrame"
instance Frame a => Eq (NewFrame a) where
  (==) _ _ = True


data X86Frame = X86Frame { x86_name :: Tmp.Label
                         , x86_formals :: [Access]
                         , x86_locals :: [Access]
                         , x86_nextOffset :: Int
                         }
                         deriving (Show, Eq)

newX86Frame :: Tmp.Label -> [Var] -> Sym.SymbolTable -> (Sym.SymbolTable, X86Frame)
newX86Frame n fs tbl =
    let
      frame = X86Frame { x86_name = n
                       , x86_formals = []
                       , x86_locals = []
                       , x86_nextOffset = 8
                       }
    in
      x86VarInit fs tbl frame

x86VarInit :: [Var] -> Sym.SymbolTable -> X86Frame -> (Sym.SymbolTable, X86Frame)
x86VarInit fs tbl frame = foldl (\(tbl', frame') f -> x86_allocFormal f frame' tbl')
                                (tbl, frame)
                                fs

x86_allocFormal :: Var -> X86Frame -> Sym.SymbolTable -> (Sym.SymbolTable, X86Frame)
x86_allocFormal = x86_alloc x86_addFormal

x86_allocLocal :: Var -> X86Frame -> Sym.SymbolTable -> ((Sym.SymbolTable, X86Frame), Access)
x86_allocLocal var f tbl = 
    let (tbl', f') = x86_alloc x86_addLocal var f tbl
    in ((tbl', f'), last $ locals f')

x86_alloc :: (Access -> X86Frame -> X86Frame) ->
             Var ->
             X86Frame ->
             Sym.SymbolTable ->
             (Sym.SymbolTable, X86Frame)
x86_alloc add Escape f tbl = ( tbl
                             , (add (InFrame $ x86_nextOffset f) f) {
                                  x86_nextOffset = (x86_nextOffset f + 4)
                                }
                             )
x86_alloc add NoEscape f tbl =
    let (tmp, tbl') = Tmp.newVar tbl
    in ( tbl'
       , add (InReg tmp) f
       )

x86_addLocal :: Access -> X86Frame -> X86Frame
x86_addLocal a f = f { x86_locals = (x86_locals f) ++ [a] }

x86_addFormal :: Access -> X86Frame -> X86Frame
x86_addFormal a f = f { x86_formals = (x86_formals f) ++ [a] }

instance Frame X86Frame where
    name = x86_name
    formals = x86_formals
    locals = x86_locals
    allocLocal = x86_allocLocal
