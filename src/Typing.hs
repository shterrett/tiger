module Typing where

import Text.Parsec.Pos (SourcePos)
import Data.List (intersperse, foldr1)
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
    | Name (Maybe ProgramType)

instance Show ProgramType where
    show TigerInt = "Integer"
    show TigerStr = "String"
    show (Record fields) = "{" ++ show fields ++ "}"
    show (Array typ) = "[" ++ show typ ++ "]"
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
typeCheck e (Sequence _ exps) = foldr1 (\l r -> if isLeft (snd l) then l else r) (typeCheck e <$> exps)
typeCheck e (ArrayCreation pos name exp1 exp2) = (e, checkArray e pos name exp1 exp2)

checkArray :: (Sym.SymbolTable, TypeEnv) ->
              SourcePos ->
              TypeName ->
              Expression ->
              Expression ->
              Either TypeError ProgramType
checkArray env@(table, types) pos name exp1 exp2 =
    (maybeToEither (Sym.get name table >>= (\s -> Env.lookup s types)) (undeclaredType pos name)) >>=
    arrayType >>=
    elementType >>=
    lengthType
    where arrayType (Name (Just (Array t))) = Right t
          arrayType t = Left $ typeError pos [Array Unit] (Right t)
          elementType t = case typeCheck env exp2 of
                            (_, Right elemType) -> if elemType == t
                                                     then Right $ Array t
                                                     else Left $ typeError pos [t] (Right elemType)
                            (_, e) -> e
          lengthType t = case typeCheck env exp1 of
                           (_, Right TigerInt) -> Right t
                           (_, mismatch) -> Left $ typeError pos [TigerInt] mismatch

maybeToEither :: Maybe ProgramType -> TypeError -> Either TypeError ProgramType
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
