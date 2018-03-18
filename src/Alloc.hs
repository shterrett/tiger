module Alloc where

import Data.List (mapAccumL)
import Data.Tuple (swap)
import Text.Parsec.Pos (SourcePos)
import qualified Symbol as Sym
import qualified Environment as Env
import qualified Temporary as Tmp
import qualified Frame
import qualified AST
import FrameExp ( Level(..)
                , Access(..)
                , LEnv(..)
                , FExp(..)
                , LValue(..)
                , Declaration(..)
                , VarEnv(..)
                , valueDec
                )

newLevel :: Frame.Frame a =>
            Frame.NewFrame a ->
            Level a ->
            Tmp.Label ->
            [Frame.Var] ->
            Sym.SymbolTable ->
            (Sym.SymbolTable, Level a)
newLevel newFrame l n fs tbl =
    let
      frameBuilder = Frame.getFn newFrame
      frame = frameBuilder n (Frame.Escape:fs) tbl
    in (Nested l) <$> frame

formals :: Frame.Frame a => Level a -> [Access a]
formals (Outermost _) = []
formals l@(Nested _ frame) = (Access l) <$>  drop 1 (Frame.formals frame)

allocLocal :: Frame.Frame a =>
              Frame.Var ->
              Level a ->
              Sym.SymbolTable ->
              ((Sym.SymbolTable, Level a), Access a)
allocLocal v (Outermost frame) tbl =
    let
      ((tbl', frame'), local) = Frame.allocLocal v frame tbl
      level = Outermost frame'
    in ((tbl', level), Access level local)
allocLocal v (Nested parent frame) tbl =
    let
      ((tbl', frame'), local) = Frame.allocLocal v frame tbl
      level = Nested parent frame'
    in ((tbl', level), Access level local)

locals :: Frame.Frame a => Level a -> [Access a]
locals l@(Outermost frame) = (Access l) <$> (Frame.locals frame)
locals l@(Nested _ frame) = (Access l) <$> (Frame.locals frame)

alloc :: Frame.Frame a => LEnv a -> AST.Expression -> FExp a
alloc le (AST.Nil pos) = Nil pos le
alloc le (AST.ValuelessExpression pos exp) = ValuelessExpression pos le (alloc le exp)
alloc le (AST.NoValue pos) = NoValue pos le
alloc le (AST.Break pos) = Break pos le
alloc le (AST.IntLiteral pos int) = IntLiteral pos le int
alloc le (AST.StringLiteral pos str) = StringLiteral pos le str
alloc le (AST.Negation pos exp) = Negation pos le (alloc le exp)
alloc le (AST.BinOp pos op exp1 exp2) = BinOp pos le op (alloc le exp1) (alloc le exp2)
alloc le (AST.Grouped pos exp) = Grouped pos le (alloc le exp)
alloc le (AST.Sequence pos exps) = Sequence pos le (fmap (alloc le) exps)
alloc le (AST.ArrayCreation pos name exp1 exp2) = ArrayCreation pos le name (alloc le exp1) (alloc le exp2)
alloc le (AST.RecordCreation pos name fields) = RecordCreation pos le name ((fmap . fmap) (alloc le) fields)
alloc le (AST.LValExp pos lval) = LValExp pos le (allocLVal le lval)
alloc le (AST.Assignment pos lval exp) = Assignment pos le (allocLVal le lval) (alloc le exp)
alloc le (AST.FunctionCall pos fn exps) = FunctionCall pos le fn (fmap (alloc le) exps)
alloc le (AST.IfThenElse pos bool truthy falsey) = IfThenElse pos le (alloc le bool) (alloc le truthy) (alloc le falsey)
alloc le (AST.IfThen pos bool truthy) = IfThen pos le (alloc le bool) (alloc le truthy)
alloc le (AST.For pos idx fm to body) = allocFor pos le idx fm to body
alloc le (AST.While pos bool body) = While pos le (alloc le bool) (alloc le body)
alloc le (AST.Let pos decs exps) = allocLet le pos decs exps

allocLVal :: Frame.Frame a => LEnv a -> AST.LValue -> LValue a
allocLVal _ (AST.Id name) = Id name
allocLVal le (AST.RecordAccess lval name) = RecordAccess (allocLVal le lval) name
allocLVal le (AST.ArraySubscript lval exp) = ArraySubscript (allocLVal le lval) (alloc le exp)

allocLet :: Frame.Frame a =>
            LEnv a ->
            SourcePos ->
            [AST.Declaration] ->
            [AST.Expression] ->
            FExp a
allocLet le pos decs exps =
    let
      valueDecs = filter valueDec decs
      (le', decs') = mapAccumL allocDec le valueDecs
    in
      Let pos le' decs' (fmap (alloc le') exps)

allocDec :: Frame.Frame a => LEnv a -> AST.Declaration -> (LEnv a, Declaration a)
allocDec le (AST.VarDec name typ exp) = allocVar le name exp
allocDec le (AST.FnDec name fields typ exp) = allocFn le name fields exp
allocDec _ (AST.TypeDec _ _) = undefined

allocVar :: Frame.Frame a =>
            LEnv a ->
            AST.Atom ->
            AST.Expression ->
            (LEnv a, Declaration a)
allocVar le name exp =
    let
      (le', _) = addLocalBinding le name
    in
      (le', VarDec name (alloc le' exp))

allocFn :: Frame.Frame a =>
           LEnv a ->
           AST.Atom ->
           AST.TypeFields ->
           AST.Expression ->
           (LEnv a, Declaration a)
allocFn (LEnv newFrame env level) name fields body =
    let
      formalNames = fmap fst fields
      (label, table) = Tmp.newLabel (sym env)
      (fn, table') = Sym.put name table
      (table'', level') = newLevel newFrame level label (fmap (const Frame.Escape) fields) table'
      (table''', varEnv) = pushFormals formalNames level' table'' (vEnv env)
      le' = LEnv newFrame
                 (env { sym = table'''
                      , vEnv = varEnv
                      })
                 level'
    in
      ( fst $ addLocalBinding (LEnv newFrame (env { sym = table''' }) level) name
      , FnDec le' name formalNames (alloc le' body))

pushFormals :: Frame.Frame a =>
               [AST.Atom] ->
               Level a ->
               Sym.SymbolTable ->
               Env.Environment (Access a) ->
               (Sym.SymbolTable, Env.Environment (Access a))
pushFormals names level table varEnv =
    let
      (table', symbols) = mapAccumL (\t n -> swap $ Sym.put n t) table names
    in
      (table', Env.pushScope (zip symbols (formals level)) varEnv)

allocFor :: Frame.Frame a =>
            SourcePos ->
            LEnv a ->
            AST.Atom ->
            AST.Expression ->
            AST.Expression ->
            AST.Expression ->
            FExp a
allocFor pos le idx fm to exp =
    let
      (le', _) = addLocalBinding le idx
    in
      For pos le' idx (alloc le' fm) (alloc le' to) (alloc le' exp)

addLocalBinding :: Frame.Frame a =>
                   LEnv a ->
                   AST.Atom ->
                   (LEnv a, Sym.Symbol)
addLocalBinding (LEnv newFrame env level) name =
    let
      ((table, level'), access) = allocLocal Frame.Escape level (sym env)
      (var, table') = Sym.put name table
      le = LEnv newFrame
                (env { vEnv = Env.addBinding (var, access) (vEnv env)
                     , sym = table'
                     })
                level'
    in
      (le, var)

