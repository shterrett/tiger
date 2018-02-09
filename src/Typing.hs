module Typing where

import Text.Parsec.Pos (SourcePos)
import Control.Monad (foldM)
import Data.List (intersperse, foldr1, foldl', sortBy, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import TigerTypes ( Expression(..)
                  , LValue(..)
                  , Declaration(..)
                  , Type(..)
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
    | Function [ProgramType] ProgramType

instance Show ProgramType where
    show TigerInt = "Integer"
    show TigerStr = "String"
    show (Record fields) = "{" ++ show fields ++ "}"
    show (Array typ) = "[" ++ show typ ++ "]"
    show Typing.Nil = "nil"
    show Unit = "()"
    show (Name name _) = show name
    show (Function args return) = "fn" ++
                                  "(" ++ show args ++ ")" ++
                                  " : " ++ show return

instance Eq ProgramType where
    (==) (TigerInt) (TigerInt) = True
    (==) (TigerStr) (TigerStr) = True
    (==) Typing.Nil Typing.Nil = True
    (==) Unit Unit = True
    (==) (Name n _) (Name m _) = n == m
    (==) _ _ = False

data TypeEnv = TypeEnv { tEnv :: Env.Environment ProgramType
                       , vEnv :: Env.Environment ProgramType
                       , sym :: Sym.SymbolTable
                       }
  deriving(Show, Eq)

type TypeError = String

data Declarable = Field
               | Identifier
               | Type
instance Show Declarable where
    show Field = "field"
    show Identifier = "identifier"
    show Type = "type"

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
typeCheck e (LValExp pos (Id name)) = lookupValue e name pos
typeCheck e (LValExp pos (RecordAccess record field)) = checkRecordAccess e pos record field
typeCheck e (LValExp pos (ArraySubscript arr sub)) = checkArraySubscript e pos arr sub
typeCheck e (DecExp pos (TypeDec name typ)) = declareType e pos name typ
typeCheck e (DecExp pos (VarDec name typ exp)) = declareVariable e pos name typ exp
typeCheck e (DecExp pos (FnDec name fields retTyp body)) = declareFn e pos name fields retTyp body
typeCheck e (Assignment pos (Id var) exp) = checkVariableAssignment e pos var exp
typeCheck e (Assignment pos (ArraySubscript arr sub) exp) = checkArrayAssignment e pos arr sub exp
typeCheck e (Assignment pos (RecordAccess rec field) exp) = checkRecordAssignment e pos rec field exp
typeCheck e (FunctionCall pos fn exps) = checkFunctionCall e pos fn exps
typeCheck e (IfThenElse pos bool truthy falsey) = checkIfThenElse e pos bool truthy falsey
typeCheck e (IfThen pos bool truthy) = checkIfThen e pos bool truthy
typeCheck e (For pos idx fm to body) = checkFor e pos idx fm to body
typeCheck e (While pos bool body) = checkWhile e pos bool body

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
          checkArrayType alias@(env', Name _ (Just typ@(Name _ _))) =
            const alias <$> checkArrayType (env', typ)
          checkArrayType mismatch = Left $ typeError pos [Array Unit] (Right mismatch)
          checkLengthExp (ev, arrType) =
            (fmap . fmap) (const arrType) $ verifyType ev TigerInt lengthExp
          checkScalarExp typ@(ev, arrType@((Name _ (Just (Array expected))))) =
            (fmap . fmap) (const arrType) $ verifyType ev expected initExp
          checkScalarExp (ev, alias@((Name _ (Just arrTyp@(Name _ _))))) =
            (fmap . fmap) (const alias) $ checkScalarExp (ev, arrTyp)

checkRecord :: TypeEnv
               -> SourcePos
               -> TypeName
               -> [(Atom, Expression)]
               -> Either TypeError (TypeEnv, ProgramType)
checkRecord env pos name fields =
    (lookupType env name pos) >>= checkRecordType >>= verifyTypesPresent >>= checkFieldsTypes
    where checkRecordType typ@(_, Name _ (Just (Record _))) = Right typ
          checkRecordType (env', alias@(Name _ (Just typ@(Name _ _)))) =
            (fmap . fmap) (const alias) $ checkRecordType (env', typ)
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
          verifyTypesPresent (env', alias@(Name _ (Just typ@(Name _ _)))) =
            (fmap . fmap) (const alias) $ verifyTypesPresent (env', typ)
          checkFieldsTypes typ@(_, Name _ (Just (Record typeFields))) =
            let exps = snd <$> (sortBy (comparing fst) fields)
                types = snd <$> (sortBy (comparing fst ) typeFields)
            in const typ <$>
               foldM (\(e', _) (exp, typ) -> verifyType e' typ exp) typ (zip exps types)
          checkFieldsTypes (env', alias@(Name _ (Just typ@(Name _ _)))) =
            (fmap . fmap) (const alias) $ checkFieldsTypes (env', typ)


checkRecordAccess :: TypeEnv
                     -> SourcePos
                     -> LValue
                     -> Atom
                     -> Either TypeError (TypeEnv, ProgramType)
checkRecordAccess env pos (Id record) field =
    lookupValue env record pos >>= recordFields pos >>= recordFieldType pos field
checkRecordAccess env pos (RecordAccess record' field') field =
    checkRecordAccess env pos record' field' >>= recordFields pos >>= recordFieldType pos field
checkRecordAccess env pos (ArraySubscript array exp) field =
   arrayElementType env pos array >>= recordFields pos >>= recordFieldType pos field

recordFields :: SourcePos -> (TypeEnv, ProgramType) -> Either TypeError (TypeEnv, [(Atom, ProgramType)])
recordFields pos (env, (Record fields)) = Right (env, fields)
recordFields pos (env, (Name _ (Just (Record fields)))) = Right (env, fields)
recordFields pos (env, (Name _ (Just typ@(Name _ _)))) = recordFields pos (env, typ)
recordFields pos mismatch = Left $ typeError pos [Record []] (Right mismatch)

recordFieldType :: SourcePos -> Atom -> (TypeEnv, [(Atom, ProgramType)]) -> Either TypeError (TypeEnv, ProgramType)
recordFieldType pos field (env, fields) = (,) env <$>
    maybeToEither (lookup field fields) (undeclaredError Field pos field)

checkArraySubscript :: TypeEnv
                       -> SourcePos
                       -> LValue
                       -> Expression
                       -> Either TypeError (TypeEnv, ProgramType)
checkArraySubscript env pos arr@(ArraySubscript arr' exp') exp =
    verifyType env TigerInt exp' >>=
      (\(env', _) -> verifyType env' TigerInt exp) >>=
      (\(env'', _) -> arrayElementType env'' pos arr)
checkArraySubscript env pos (RecordAccess record field) exp =
  verifyType env TigerInt exp >>
    checkRecordAccess env pos record field >>=
    scalarType pos
checkArraySubscript env pos lval@(Id _) exp =
    verifyType env TigerInt exp >> arrayElementType env pos lval

arrayElementType :: TypeEnv ->
                    SourcePos ->
                    LValue ->
                    Either TypeError (TypeEnv, ProgramType)
arrayElementType env pos lval = typeCheck env (LValExp pos lval) >>= scalarType pos

scalarType :: SourcePos -> (TypeEnv, ProgramType) -> Either TypeError (TypeEnv, ProgramType)
scalarType _ (env, Array typ) = Right (env, typ)
scalarType _ (env, Name _ (Just (Array typ))) = Right (env, typ)
scalarType pos (env, Name _ (Just typ@(Name _ _))) = scalarType pos (env, typ)
scalarType pos mismatch = Left $ typeError pos [Array Unit] (Right mismatch)

declareType :: TypeEnv
               -> SourcePos
               -> TypeName
               -> Type
               -> Either TypeError (TypeEnv, ProgramType)
declareType env pos name (TypeId typeName) =
    addTypeBinding name id <$> lookupType env typeName pos
declareType env pos name (ArrayOf typeName) =
    addTypeBinding name Array <$> lookupType env typeName pos
declareType env pos name (RecordOf fields) =
    (addTypeBinding name id) <$>
    ((,) env) <$>
    Record <$>
    recordTypeFields
    where recordTypeFields = sequence $
                            (\(fieldName, typeName) ->
                              ((,) fieldName) . snd <$>
                               (lookupType env typeName pos)) <$>
                            fields

addTypeBinding :: TypeName ->
                  (ProgramType -> ProgramType) ->
                  (TypeEnv, ProgramType) ->
                  (TypeEnv, ProgramType)
addTypeBinding name aggregateConstructor (env, typ) =
    let
      (symbol, tbl) = Sym.put name (sym env)
      newType = Name symbol $ Just (aggregateConstructor typ)
    in
     ( env { tEnv = Env.addBinding (symbol, newType) (tEnv env)
           , sym = tbl
           }
     , Unit
     )

declareVariable :: TypeEnv
                   -> SourcePos
                   -> Atom
                   -> Maybe TypeName
                   -> Expression
                   -> Either TypeError (TypeEnv, ProgramType)
declareVariable e pos name Nothing exp =
    addVarBinding name <$> typeCheck e exp
declareVariable e pos name (Just typName) exp =
    addVarBinding name <$>
      (lookupType e typName pos >>= verifyType' exp)
    where verifyType' exp (e', typ) = verifyType e' typ exp

addVarBinding :: Atom -> (TypeEnv, ProgramType) -> (TypeEnv, ProgramType)
addVarBinding name (env, typ) =
    let
      (symbol, tbl) = Sym.put name (sym env)
    in
      ( env { vEnv = Env.addBinding (symbol, typ) (vEnv env)
            , sym = tbl
            }
      , Unit
      )

addVarScope :: TypeEnv -> [(Atom, ProgramType)] -> TypeEnv
addVarScope env typs =
    let
      initial = Sym.put "nil" (sym env)
      atomsAndTables = scanr (\(name, _) (_, tbl')-> Sym.put name tbl') initial typs
      table' = snd $ head atomsAndTables
      atomsAndTypes = zip (fst <$> atomsAndTables) (snd <$> typs)
    in env { vEnv = Env.pushScope atomsAndTypes (vEnv env)
           , sym = table'
           }

declareFn :: TypeEnv
             -> SourcePos
             -> Atom
             -> [(Atom, TypeName)]
             -> Maybe TypeName
             -> Expression
             -> Either TypeError (TypeEnv, ProgramType)
declareFn e pos name fields (Just retTyp) body =
    let
      args = argTypes pos e fields
      returnType = snd <$> lookupType e retTyp pos
      verifyReturnType = \(e', rt) -> verifyType e' rt body
      fnTyp = Function <$> ((fmap . fmap) snd args) <*> returnType
    in
      addVarBinding name <$>
      (
        ((((,) <$> (addVarScope e <$> args)) <*> returnType)) >>=
        verifyReturnType >>
        ((,) e) <$> fnTyp
      )
declareFn e pos name fields Nothing body =
    let
      args = argTypes pos e fields
      fnTyp = (flip Function $ Unit) <$> ((fmap . fmap) snd args)
    in
      addVarBinding name <$>
      (
        ((addVarScope e <$> args) >>= (flip typeCheck $ body)) >>
        ((,) e) <$> fnTyp
      )

argTypes :: SourcePos -> TypeEnv -> [(Atom, TypeName)] -> Either TypeError [(Atom, ProgramType)]
argTypes pos e fields =
    (zip $ fmap fst fields) <$>
    fmap snd <$>
    mapM (\(_, typName) -> lookupType e typName pos) fields

checkVariableAssignment :: TypeEnv ->
                           SourcePos ->
                           Atom ->
                           Expression ->
                           Either TypeError (TypeEnv, ProgramType)
checkVariableAssignment e pos var exp =
      lookupValue e var pos >>= checkAssignment exp

checkArrayAssignment :: TypeEnv ->
                        SourcePos ->
                        LValue ->
                        Expression ->
                        Expression ->
                        Either TypeError (TypeEnv, ProgramType)
checkArrayAssignment e pos arr sub exp =
      checkArraySubscript e pos arr sub >>= checkAssignment exp

checkRecordAssignment :: TypeEnv ->
                         SourcePos ->
                         LValue ->
                         Atom ->
                         Expression ->
                         Either TypeError (TypeEnv, ProgramType)
checkRecordAssignment e pos rec field exp =
      checkRecordAccess e pos rec field >>= checkAssignment exp

checkAssignment :: Expression ->
                   (TypeEnv, ProgramType) ->
                   Either TypeError (TypeEnv, ProgramType)
checkAssignment exp (e', expected) =
    (fmap . fmap) (const Unit) $ verifyType e' expected exp

checkFunctionCall :: TypeEnv ->
                     SourcePos ->
                     Atom ->
                     [Expression] ->
                     Either TypeError (TypeEnv, ProgramType)
checkFunctionCall e pos fn args =
    lookupValue e fn pos >>=
    verifyIsFunction >>=
    verifyNumArgs >>=
    verifyArgTypes >>=
    returnType
    where verifyIsFunction res@(_, Function _ _) = Right res
          verifyIsFunction mismatch = Left $ typeError pos [Function [] Unit] (Right mismatch)
          verifyNumArgs res@(_, Function params _)
            | length params == length args = Right res
            | otherwise = Left $ "Incorrect number of arguments: expected " ++
                          show (length params) ++
                          " given " ++
                          show (length args)
          verifyArgTypes res@(_, Function params _) =
            const res <$>
              foldM (\(e', _) (paramType, argExp) -> verifyType e' paramType argExp)
                    (e, Unit)
                    (zip params args)
          returnType (e, Function _ typ) = Right (e, typ)

checkIfThenElse :: TypeEnv ->
                   SourcePos ->
                   Expression ->
                   Expression ->
                   Expression ->
                   Either TypeError (TypeEnv, ProgramType)
checkIfThenElse e pos bool truthy falsey =
    verifyType e TigerInt bool >>
    typeCheck e truthy >>=
    (\(_, typ) -> verifyType e typ falsey)

checkIfThen :: TypeEnv ->
               SourcePos ->
               Expression ->
               Expression ->
               Either TypeError (TypeEnv, ProgramType)
checkIfThen e pos bool truthy =
    verifyType e TigerInt bool >>
    verifyType e Unit truthy

checkFor :: TypeEnv ->
            SourcePos ->
            Atom ->
            Expression ->
            Expression ->
            Expression ->
            Either TypeError (TypeEnv, ProgramType)
checkFor e pos idx fm to body =
    verifyType e TigerInt fm >>
    verifyType e TigerInt to >>
    const (e, Unit) <$> verifyType (addVarScope e [(idx, TigerInt)]) Unit body

checkWhile :: TypeEnv ->
              SourcePos ->
              Expression ->
              Expression ->
              Either TypeError (TypeEnv, ProgramType)
checkWhile e pos bool body =
    verifyType e TigerInt bool >>
    const (e, Unit) <$> verifyType e Unit body


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
lookupType env name pos =
    (maybeToEither ((,) env <$> (lookupTypeVal env name tEnv))
                              (undeclaredError Type pos name))

lookupValue :: TypeEnv ->
               Atom ->
               SourcePos ->
               Either TypeError (TypeEnv, ProgramType)
lookupValue env name pos =
    (maybeToEither ((,) env <$> (lookupTypeVal env name vEnv))
                              (undeclaredError Identifier pos name))

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing e = Left e
maybeToEither (Just t) _ = Right t

undeclaredError :: Declarable -> SourcePos -> Atom -> TypeError
undeclaredError dec pos name =
    "Type Error! Undeclared " ++ (show dec) ++ ": " ++ name ++ " at " ++ show pos

lookupTypeVal :: TypeEnv -> String -> (TypeEnv -> Env.Environment a) -> Maybe a
lookupTypeVal env name getEnv = Sym.get name (sym env) >>= (lookup (getEnv env))
  where lookup = flip Env.lookup

typeError :: SourcePos -> [ProgramType] -> Either TypeError (TypeEnv, ProgramType) -> TypeError
typeError pos expected (Right (_, actual)) =
    "Type Error! Expected " ++
    (mconcat $ intersperse " or " (show <$> expected)) ++
    " but got " ++
    (show actual) ++ 
    " at " ++
    (show pos)
typeError _ _ (Left err) = err
