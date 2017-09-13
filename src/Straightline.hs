module Straightline where

type Id = String
data Binop =
    Plus
    | Minus
    | Times
    | Div

data Exp =
    IdExp Id
    | NumExp Integer
    | OpExp Exp Binop Exp
    | EseqExp Stm Exp

data Stm =
    CompoundStm Stm Stm
    | AssignStm Id Exp
    | PrintStm [Exp]

maxargs :: Stm -> Integer
maxargs s = 0
