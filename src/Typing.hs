module Typing where

import Text.Parsec.Pos (SourcePos)
import Control.Monad (foldM)
import Data.List (intersperse, foldr1, foldl', sortBy, sort)
import Data.Either (isLeft)
import Data.Ord (comparing)
import TigerTypes ( Expression(..)
                  , Operator(..)
                  , TypeName
                  , position
                  , Atom
                  )
import qualified Symbol as Sym
import qualified Environment as Env

data ProgramType =
    TigerInt
    | TigerStr
    | Record [(Atom, ProgramType)]
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
typeCheck e (Negation pos exp) = verifyType e TigerInt exp
typeCheck e (BinOp pos op exp1 exp2) = checkBinOp e pos op exp1 exp2
typeCheck e (Grouped _ exp) = typeCheck e exp
typeCheck e (Sequence pos exps) = foldM (\(e', _) exp -> typeCheck e' exp) (e, Unit) exps
typeCheck e (ArrayCreation pos name exp1 exp2) = checkArray e pos name exp1 exp2
typeCheck e (RecordCreation pos name fields) = checkRecord e pos name fields

checkBinOp :: TypeEnv ->
              SourcePos ->
              Operator ->
              Expression ->
              Expression ->
              Either TypeError (TypeEnv, ProgramType)
checkBinOp e pos op exp1 exp2
    | op `elem` [Addition, Subtraction, Multiplication, Division, And, Or] =
      foldM (\(e', typ) exp -> verifyType e' TigerInt exp) (e, TigerInt) [exp1, exp2]
    | op `elem` [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual] =
      case (typeCheck e exp1, typeCheck e exp2) of
        ((Right (_, TigerInt)), (Right (_, TigerInt))) -> Right (e, TigerInt)
        ((Right (_, TigerStr)), (Right (_, TigerStr))) -> Right (e, TigerInt)
        ((Right (_, TigerInt)), mismatch) -> Left $ typeError pos [TigerInt] mismatch
        ((Right (_, TigerStr)), mismatch) -> Left $ typeError pos [TigerStr] mismatch
        (mismatch, _) -> Left $ typeError pos [TigerInt, TigerStr] mismatch

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
            (\(ev', _) -> (ev', arrType)) <$> verifyType ev TigerInt lengthExp
          checkScalarExp typ@(ev, arrType@((Name _ (Just (Array expected))))) =
            (\(ev', _) -> (ev', arrType)) <$> verifyType ev expected initExp

checkRecord :: TypeEnv
               -> SourcePos
               -> TypeName
               -> [(Atom, Expression)]
               -> Either TypeError (TypeEnv, ProgramType)
checkRecord env pos name fields =
    (lookupType env name pos) >>= checkRecordType >>= verifyTypesPresent >>= checkFieldsTypes
    where checkRecordType typ@(_, Name _ (Just (Record _))) = Right typ
          checkRecordType mismatch = Left $ typeError pos [Record []] (Right mismatch)
          verifyTypesPresent typ@(_, Name _ (Just (Record typeFields))) =
            let actualFields = sort (fst <$> fields)
                expectedFields = sort (fst <$> typeFields)
            in if actualFields == expectedFields
               then Right typ
               else Left $ "Type Error! Expected fields " ++
                           (mconcat $ intersperse ", " (show <$> expectedFields)) ++
                           "But got " ++
                           (mconcat $ intersperse ", " (show <$> actualFields))
          checkFieldsTypes typ@(_, Name _ (Just (Record typeFields))) =
            let exps = snd <$> (sortBy (comparing fst) fields)
                types = snd <$> (sortBy (comparing fst ) typeFields)
            in const typ <$>
               foldM (\(e', _) (exp, typ) -> verifyType e' typ exp) typ (zip exps types)

verifyType :: TypeEnv
              -> ProgramType
              -> Expression
              -> Either TypeError (TypeEnv, ProgramType)
verifyType env typ exp =
    case typeCheck env exp of
      res@(Right (env', act)) ->
        if act == typ
        then Right (env', act)
        else Left $ typeError (position exp) [typ] res
      mismatch -> Left $ typeError (position exp) [typ] mismatch

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
