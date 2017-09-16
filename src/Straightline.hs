module Straightline where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ( (!) )
import Data.Foldable (foldl')

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

type Store = Map.Map Id [Int]
type Output = [Int]
type Env = (Store, Output)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

envStore :: Id -> Int -> Env -> Env
envStore id int (store, out) = (Map.insertWith (++) id [int] store, out)

envLookup :: Env -> Id -> Int
envLookup (store, _) id = head (store ! id)

printExp :: Env -> Exp -> Env
printExp env exp =
    let (val, (s1, o1)) = interpExp exp env
    in (s1, o1 ++ [val])

operate :: Binop -> Int -> Int -> Int
operate Plus = (+)
operate Minus = (-)
operate Times = (*)
operate Div = div

interp :: Stm -> IO ()
interp stm = let (_, output) = interpStm stm (Map.empty, [])
             in (putStrLn . show) output

interpStm ::  Stm -> Env -> Env
interpStm (CompoundStm s1 s2) e =
    interpStm s2 $ interpStm s1 e
interpStm (AssignStm id exp) e =
    uncurry (envStore id) $ interpExp exp e
interpStm (PrintStm exps) e =
    foldl' printExp e exps

interpExp :: Exp -> Env -> (Int, Env)
interpExp (IdExp id) e = (envLookup e id, e)
interpExp (NumExp int) e = (int, e)
interpExp (OpExp exp1 op exp2) e =
    let (val1, _) = interpExp exp1 e
        (val2, _) = interpExp exp2 e
    in (operate op val1 val2, e)
interpExp (EseqExp stm exp) e =
    interpExp exp $ interpStm stm e
