module Typing where

import Text.Parsec.Pos (SourcePos)
import TigerTypes (Expression(..), Operator(..), TypeFields, TypeName)
import qualified Symbol as Sym
import qualified Environment as Env

data ProgramType =
    TigerInt
    | TigerStr
    | Record TypeFields
    | Array TypeName
    | Nil
    | Unit
    | Name TypeName

instance Show ProgramType where
    show TigerInt = "Integer"
    show TigerStr = "String"
    show (Record fields) = "{" ++ show fields ++ "}"
    show (Array name) = "[" ++ show name ++ "]"
    show Typing.Nil = "nil"
    show Unit = "()"
    show (Name name) = show name

instance Eq ProgramType where
    (==) (TigerInt) (TigerInt) = True
    (==) (TigerStr) (TigerStr) = True
    (==) Typing.Nil Typing.Nil = True
    (==) Unit Unit = True
    (==) (Name n) (Name m) = n == m
    (==) _ _ = False

type TypeEnv = Env.Environment ProgramType
type TypeError = String

typeCheck :: (Sym.SymbolTable, TypeEnv) -> Expression -> ((Sym.SymbolTable, TypeEnv), Either TypeError ProgramType)
typeCheck e (TigerTypes.Nil _) = (e, Right Typing.Nil)
typeCheck e (ValuelessExpression _ _) = (e, Right Unit)
typeCheck e (NoValue _) = (e, Right Unit)
typeCheck e (IntLiteral _ _) = (e, Right TigerInt)
typeCheck e (StringLiteral _ _) = (e, Right TigerStr)
typeCheck e (Negation pos exp) =
    case typeCheck e exp of
      (_, Right TigerInt) -> (e, Right TigerInt)
      (_, mismatch) -> (e, Left $ typeError pos TigerInt mismatch)
typeCheck e (BinOp pos op exp1 exp2)
    | op `elem` [Addition, Subtraction, Multiplication, Division, And, Or] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((_, Right TigerInt), (_, Right TigerInt)) -> (e, Right TigerInt)
        ((_, mismatch), (_, Right TigerInt)) -> (e, Left $ typeError pos TigerInt mismatch)
        ((_, Right TigerInt), (_, mismatch)) -> (e, Left $ typeError pos TigerInt mismatch)
        ((_, mismatch), (_, _)) -> (e, Left $ typeError pos TigerInt mismatch)
    | op `elem` [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((_, Right TigerInt), (_, Right TigerInt)) -> (e, Right TigerInt)
        ((_, Right TigerStr), (_, Right TigerStr)) -> (e, Right TigerInt)
        ((_, Right TigerInt), (_, mismatch)) -> (e, Left $ typeError pos TigerInt mismatch)
        ((_, Right TigerStr), (_, mismatch)) -> (e, Left $ typeError pos TigerStr mismatch)
        ((_, mismatch), (_, _)) -> (e, Left $ typeError pos TigerInt mismatch)

typeError :: SourcePos -> ProgramType -> Either TypeError ProgramType -> TypeError
typeError pos expected (Right actual) =
    "Type Error! Expected " ++
    (show expected) ++
    " but got " ++
    (show actual) ++ 
    " at " ++
    (show pos)
typeError _ _ (Left err) = err
