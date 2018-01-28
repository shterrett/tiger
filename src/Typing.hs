module Typing where

import Text.Parsec.Pos (SourcePos)
import Control.Monad (foldM)
import Data.List (intersperse, foldr1, foldl')
import Data.Either (isLeft)
import TigerTypes (Expression(..), Operator(..), TypeFields, TypeName, position)
import qualified Symbol as Sym
import qualified Environment as Env

data ProgramType =
    TigerInt
    | TigerStr
    | Record TypeFields
    | Array ProgramType
    | Nil
    | Unit
    | Name Sym.Symbol (Maybe ProgramType)

instance Show ProgramType where
    show TigerInt = "Integer"
    show TigerStr = "String"
    show (Record fields) = "{" ++ show fields ++ "}"
    show (Array typ) = "[" ++ show typ ++ "]"
    show Typing.Nil = "nil"
    show Unit = "()"
    show (Name name _) = show name

instance Eq ProgramType where
    (==) (TigerInt) (TigerInt) = True
    (==) (TigerStr) (TigerStr) = True
    (==) Typing.Nil Typing.Nil = True
    (==) Unit Unit = True
    (==) (Name n _) (Name m _) = n == m
    (==) _ _ = False

type TypeEnv = (Sym.SymbolTable, Env.Environment ProgramType)
type TypeError = String

typeCheck :: TypeEnv -> Expression -> Either TypeError (TypeEnv, ProgramType)
typeCheck e (TigerTypes.Nil _) = Right (e, Typing.Nil)
typeCheck e (ValuelessExpression _ _) = Right (e, Unit)
typeCheck e (NoValue _) = Right (e, Unit)
typeCheck e (IntLiteral _ _) = Right (e, TigerInt)
typeCheck e (StringLiteral _ _) = Right (e, TigerStr)
typeCheck e (Negation pos exp) =
    case typeCheck e exp of
      success@(Right (_, TigerInt)) -> success
      mismatch -> Left $ typeError pos [TigerInt] mismatch
typeCheck e (BinOp pos op exp1 exp2)
    | op `elem` [Addition, Subtraction, Multiplication, Division, And, Or] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((Right (_, TigerInt)), (Right (_, TigerInt))) -> Right (e, TigerInt)
        (mismatch, (Right (_, TigerInt))) -> Left $ typeError pos [TigerInt] mismatch
        ((Right (_, TigerInt)), mismatch) -> Left $ typeError pos [TigerInt] mismatch
        (mismatch, _) -> Left $ typeError pos [TigerInt] mismatch
    | op `elem` [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((Right (_, TigerInt)), (Right (_, TigerInt))) -> Right (e, TigerInt)
        ((Right (_, TigerStr)), (Right (_, TigerStr))) -> Right (e, TigerInt)
        ((Right (_, TigerInt)), mismatch) -> Left $ typeError pos [TigerInt] mismatch
        ((Right (_, TigerStr)), mismatch) -> Left $ typeError pos [TigerStr] mismatch
        (mismatch, _) -> Left $ typeError pos [TigerInt, TigerStr] mismatch
typeCheck e (Grouped _ exp) = typeCheck e exp
typeCheck e (Sequence pos exps) = foldM (\(e', _) exp -> typeCheck e' exp) (e, Unit) exps
typeCheck e (ArrayCreation pos name exp1 exp2) = checkArray e pos name exp1 exp2

checkArray :: TypeEnv ->
              SourcePos ->
              TypeName ->
              Expression ->
              Expression ->
              Either TypeError (TypeEnv, ProgramType)
checkArray env pos name lengthExp initExp =
    (lookupType env name pos) >>= checkArrayType >>= checkLengthExp >>= checkScalarExp
    where checkArrayType typ@(_, Name _ (Just (Array _))) = Right typ
          checkArrayType mismatch = Left $ typeError pos [Array Unit] (Right mismatch)
          checkLengthExp (ev, arrType) =
            case typeCheck ev lengthExp of
              (Right (ev', TigerInt)) -> Right (ev', arrType)
              mismatch -> Left $ typeError (position lengthExp) [TigerInt] mismatch
          checkScalarExp typ@(ev, arrType@((Name _ (Just (Array expected))))) =
            case typeCheck ev initExp of
              scalarResult@(Right (ev', scalarType)) -> if expected == scalarType
                                                          then Right typ
                                                          else Left $ typeError (position initExp) [expected] scalarResult
              mismatch -> Left $ typeError pos [expected] mismatch

lookupType :: TypeEnv ->
              TypeName ->
              SourcePos ->
              Either TypeError (TypeEnv, ProgramType)
lookupType env@(table, ev) name pos =
    (maybeToEither ((,) env <$> (Sym.get name table >>= (lookup ev)))
                              (undeclaredType pos name))
    where lookup = flip Env.lookup
          maybeToEither Nothing e = Left e
          maybeToEither (Just t) _ = Right t

undeclaredType :: SourcePos -> TypeName -> TypeError
undeclaredType pos name =
    "Type Error! Undeclared Type " ++ name ++ " at" ++ show pos

typeError :: SourcePos -> [ProgramType] -> Either TypeError (TypeEnv, ProgramType) -> TypeError
typeError pos expected (Right (_, actual)) =
    "Type Error! Expected " ++
    (mconcat $ intersperse " or " (show <$> expected)) ++
    " but got " ++
    (show actual) ++ 
    " at " ++
    (show pos)
typeError _ _ (Left err) = err
