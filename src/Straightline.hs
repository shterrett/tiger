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

envStore :: Env -> Id -> Int -> Env
envStore (store, out) id int = (Map.insertWith (++) id [int] store, out)

envLookup :: Env -> Id -> Int
envLookup (store, _) id = head (store ! id)

printExp :: Env -> Exp -> Env
printExp (store, out) exp =
    let ((s1, o1), val) = interpExp (store, out) exp
    in (s1, o1 ++ [val])

operate :: Binop -> Int -> Int -> Int
operate Plus = (+)
operate Minus = (-)
operate Times = (*)
operate Div = div

interp :: Stm -> IO ()
interp stm = let (_, output) = interpStm (Map.empty, []) stm
             in (putStrLn . show) output

interpStm :: Env -> Stm -> Env
interpStm e (CompoundStm s1 s2) = let e2 = interpStm e s1
                                  in interpStm e2 s2
interpStm e (AssignStm id exp) =
    let (e2, val) = interpExp e exp
    in envStore e2 id val
interpStm (store, out) (PrintStm exps) =
    foldl' printExp (store, out) exps

interpExp :: Env -> Exp -> (Env, Int)
interpExp e (IdExp id) = (e, envLookup e id)
interpExp e (NumExp int) = (e, int)
interpExp e (OpExp exp1 op exp2) =
    let (_, val1) = interpExp e exp1
        (_, val2) = interpExp e exp2
    in (e, operate op val1 val2)
interpExp e (EseqExp stm exp) =
    let e2 = interpStm e stm
    in interpExp e2 exp
