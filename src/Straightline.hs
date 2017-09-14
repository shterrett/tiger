module Straightline where

type Id = String
data Binop =
    Plus
    | Minus
    | Times
    | Div

data Exp =
    IdExp Id
    | NumExp Int
    | OpExp Exp Binop Exp
    | EseqExp Stm Exp

data Stm =
    CompoundStm Stm Stm
    | AssignStm Id Exp
    | PrintStm [Exp]

maxargs :: Stm -> Int
maxargs s = stmMaxPrintArgs 0 s

stmMaxPrintArgs :: Int -> Stm -> Int
stmMaxPrintArgs m (CompoundStm s1 s2) = max m $ max (stmMaxPrintArgs m s1) (stmMaxPrintArgs m s2)
stmMaxPrintArgs m (PrintStm es) = max m $
                                  max (length es) (maximum $ (expMaxPrintArgs m) <$> es)
stmMaxPrintArgs m (AssignStm _ e) = max m $ expMaxPrintArgs m e

expMaxPrintArgs :: Int -> Exp -> Int
expMaxPrintArgs m (IdExp _) = m
expMaxPrintArgs m (NumExp _) = m
expMaxPrintArgs m (OpExp e1 _ e2) = max m $
                                        max (expMaxPrintArgs m e1) (expMaxPrintArgs m e2)
expMaxPrintArgs m (EseqExp s e) = max (stmMaxPrintArgs m s) (expMaxPrintArgs m e)
