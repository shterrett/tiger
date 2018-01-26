module Typing where

import Text.Parsec.Pos (SourcePos)
import Data.List (intersperse, foldr1, foldl')
import Data.Either (isLeft)
import TigerTypes (Expression(..), Operator(..), TypeFields, TypeName)
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
      (_, mismatch) -> (e, Left $ typeError pos [TigerInt] mismatch)
typeCheck e (BinOp pos op exp1 exp2)
    | op `elem` [Addition, Subtraction, Multiplication, Division, And, Or] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((_, Right TigerInt), (_, Right TigerInt)) -> (e, Right TigerInt)
        ((_, mismatch), (_, Right TigerInt)) -> (e, Left $ typeError pos [TigerInt] mismatch)
        ((_, Right TigerInt), (_, mismatch)) -> (e, Left $ typeError pos [TigerInt] mismatch)
        ((_, mismatch), (_, _)) -> (e, Left $ typeError pos [TigerInt] mismatch)
    | op `elem` [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((_, Right TigerInt), (_, Right TigerInt)) -> (e, Right TigerInt)
        ((_, Right TigerStr), (_, Right TigerStr)) -> (e, Right TigerInt)
        ((_, Right TigerInt), (_, mismatch)) -> (e, Left $ typeError pos [TigerInt] mismatch)
        ((_, Right TigerStr), (_, mismatch)) -> (e, Left $ typeError pos [TigerStr] mismatch)
        ((_, mismatch), (_, _)) -> (e, Left $ typeError pos [TigerInt, TigerStr] mismatch)
typeCheck e (Grouped _ exp) = typeCheck e exp
typeCheck e (Sequence _ []) = (e, Right Unit)
typeCheck e (Sequence _ exps) = foldl' checkSeqElem (e, Right Unit) exps
typeCheck e (ArrayCreation pos name exp1 exp2) = checkArray e pos name exp1 exp2

checkSeqElem :: ((Sym.SymbolTable, TypeEnv), Either TypeError ProgramType) ->
                Expression ->
                ((Sym.SymbolTable, TypeEnv), Either TypeError ProgramType)
checkSeqElem (ev, Right typ) exp = typeCheck ev exp
checkSeqElem result _ = result

checkArray :: (Sym.SymbolTable, TypeEnv) ->
              SourcePos ->
              TypeName ->
              Expression ->
              Expression ->
              ((Sym.SymbolTable, TypeEnv), Either TypeError ProgramType)
checkArray env pos name lengthExp initExp =
    foldl' (flip ($))
           (lookupType env name pos)
           [checkArray, checkLengthExp, checkScalarExp]
    where checkArray (ev, arrType@(Right ( Name _ (Just (Array expected))))) = (ev, arrType)
          checkArray (ev, mismatch) = (ev, Left $ typeError pos [Array Unit] mismatch)
          checkLengthExp (ev, arrType) = case typeCheck ev lengthExp of
                                           (ev', Right TigerInt) -> (ev', arrType)
                                           (ev', mismatch) -> (ev', Left $ typeError pos [TigerInt] mismatch)
          checkScalarExp (ev, arrType@(Right (Name _ (Just (Array expected))))) = case typeCheck ev initExp of
                                           (ev', Right scalarType) -> if expected == scalarType
                                                                        then (ev', arrType)
                                                                        else (ev', Left $ typeError pos [expected] (Right scalarType))
                                           (ev', mismatch) -> (ev', Left $ typeError pos [expected] mismatch)
lookupType :: (Sym.SymbolTable, TypeEnv) ->
              TypeName ->
              SourcePos ->
              ((Sym.SymbolTable, TypeEnv), Either TypeError ProgramType)
lookupType env@(table, ev) name pos =
    ( env
    , (maybeToEither (Sym.get name table >>= (lookup ev))
                     (undeclaredType pos name))
    )
    where lookup = flip Env.lookup
          maybeToEither Nothing e = Left e
          maybeToEither (Just t) _ = Right t

undeclaredType :: SourcePos -> TypeName -> TypeError
undeclaredType pos name =
    "Type Error! Undeclared Type " ++ name ++ " at" ++ show pos

typeError :: SourcePos -> [ProgramType] -> Either TypeError ProgramType -> TypeError
typeError pos expected (Right actual) =
    "Type Error! Expected " ++
    (mconcat $ intersperse " or " (show <$> expected)) ++
    " but got " ++
    (show actual) ++ 
    " at " ++
    (show pos)
typeError _ _ (Left err) = err
