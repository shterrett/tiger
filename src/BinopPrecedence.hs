module BinopPrecedence where

import TigerTypes

setPrecedence :: Expression -> Expression
setPrecedence root@(BinOp pos1 rootOp l@(BinOp pos2 opL ll lr) r) =
    if rootOp > opL
    then BinOp pos2 opL ll (BinOp pos1 rootOp lr r)
    else root
setPrecedence exp = exp
