module BinopPrecedence where

import AST

setPrecedence :: Expression -> Expression
setPrecedence root@(BinOp pos1 rootOp l@(BinOp _ _ _ _) r) =
    let
      l'@(BinOp pos2 opL ll lr) = setPrecedence l
    in
      if rootOp > opL
      then BinOp pos2 opL ll (setPrecedence $ BinOp pos1 rootOp lr r)
      else root
setPrecedence exp = exp
