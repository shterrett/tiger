module Typing where

import Text.Parsec.Pos (SourcePos)
import Control.Monad (foldM)
import Data.List ( intersperse
                 , foldr1
                 , foldl'
                 , nub
                 , (\\)
                 , sortBy
                 , sort
                 )
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import AST ( Expression(..)
           , LValue(..)
           , Declaration(..)
           , Type(..)
           , Operator(..)
           , TypeName
           , position
           , Atom
           )
import Types ( TypeInfo
             , ProgramType(..)
             , TypeEnv(..)
             , TypeError
             , mapEnv
             , mapType
             , TExp
             , getType
             , getTEnv
             , typeInfo
             , Declarable(..)
             , isNullable
             )
import qualified Types as T
import qualified Symbol as Sym
import qualified Environment as Env

typeCheck :: TypeEnv -> Expression -> Either TypeError TExp
typeCheck e (Nil pos) = Left $ "Cannot infer the type of nil at " ++ show pos
typeCheck e (ValuelessExpression pos exp) = T.ValuelessExpression pos (e, Unit) <$> typeCheck e exp
typeCheck e (NoValue pos) = Right $ T.NoValue pos (e, Unit)
typeCheck e (Break pos) = Right $ T.Break pos (e, Unit)
typeCheck e (IntLiteral pos int) = Right $ T.IntLiteral pos (e, TigerInt) int
typeCheck e (StringLiteral pos str) = Right $ T.StringLiteral pos (e, TigerStr) str
typeCheck e (Negation pos exp) = (T.Negation pos (e, TigerInt) <$> verifyType e TigerInt exp)
typeCheck e (BinOp pos op exp1 exp2) = checkBinOp e pos op exp1 exp2
typeCheck e (Grouped pos exp) =
    (\texp -> T.Grouped pos (typeInfo texp) texp) <$>
      typeCheck e exp
typeCheck e (Sequence pos exps) =
    (sequence $ fmap (typeCheck e) exps) >>=
      (\texps -> return $ T.Sequence pos (typeInfo . last $ texps) texps)
typeCheck e (ArrayCreation pos name exp1 exp2) = checkArray e pos name exp1 exp2
typeCheck e (RecordCreation pos name fields) = checkRecord e pos name fields
typeCheck e (LValExp pos lval@(Id name)) =
    T.LValExp pos <$> lookupValue e name pos <*> (checkLVal e lval)
typeCheck e (LValExp pos (RecordAccess record field)) =
    (uncurry $ T.LValExp pos) <$> checkRecordAccess e pos record field
typeCheck e (LValExp pos (ArraySubscript arr sub)) =
    (uncurry $ T.LValExp pos) <$> checkArraySubscript e pos arr sub
typeCheck e (Assignment pos (Id var) exp) = checkVariableAssignment e pos var exp
typeCheck e (Assignment pos (ArraySubscript arr sub) exp) = checkArrayAssignment e pos arr sub exp
typeCheck e (Assignment pos (RecordAccess rec field) exp) = checkRecordAssignment e pos rec field exp
typeCheck e (FunctionCall pos fn exps) = checkFunctionCall e pos fn exps
typeCheck e (IfThenElse pos bool truthy falsey) = checkIfThenElse e pos bool truthy falsey
typeCheck e (IfThen pos bool truthy) = checkIfThen e pos bool truthy
typeCheck e (For pos idx fm to body) = checkFor e pos idx fm to body
typeCheck e (While pos bool body) = checkWhile e pos bool body
typeCheck e (Let pos decs exps) = checkLet e pos decs exps

checkLVal :: TypeEnv -> LValue -> Either TypeError T.LValue
checkLVal _ (Id name) = Right $ T.Id name
checkLVal e (RecordAccess lval fields) =
    (\lval' -> T.RecordAccess lval' fields) <$>
      checkLVal e lval
checkLVal e (ArraySubscript lval exp) = T.ArraySubscript <$> (checkLVal e lval) <*> typeCheck e exp

checkBinOp :: TypeEnv ->
              SourcePos ->
              Operator ->
              Expression ->
              Expression ->
              Either TypeError TExp
checkBinOp e pos op exp1 exp2
    | op `elem` [Addition, Subtraction, Multiplication, Division, And, Or] =
      T.BinOp pos (e, TigerInt) op <$> verifyType e TigerInt exp1 <*> verifyType e TigerInt exp2
    | op `elem` [LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual] =
      let
        leftTyp = typeCheck e exp1
      in
        case (typeInfo <$> leftTyp) of
          (Right typ@(_, TigerInt)) -> cmpTyp typ leftTyp exp2
          (Right typ@(_, TigerStr)) -> cmpTyp typ leftTyp exp2
          mismatch -> Left $ typeError pos [TigerInt, TigerStr] mismatch
    | op `elem` [Equality, NonEquality] =
      let
        leftTyp = typeCheck e exp1
      in
        case (typeInfo <$> leftTyp) of
          (Right typ@(_, rt)) -> cmpTyp typ leftTyp exp2
          mismatch -> Left $ typeError pos [Unit] mismatch
    where
      cmpTyp (e', _) leftTyp rightExp = T.BinOp pos (e', TigerInt) op <$>
                      leftTyp <*>
                       (snd . typeInfo <$> leftTyp >>= (flip (verifyType e) $ rightExp))

checkArray :: TypeEnv ->
              SourcePos ->
              TypeName ->
              Expression ->
              Expression ->
              Either TypeError TExp
checkArray env pos name lengthExp initExp =
    let
      typ = (lookupType env name pos) >>= checkArrayType
    in
     (\t -> T.ArrayCreation pos t name) <$> typ <*> (typ >>= checkLengthExp) <*> (typ >>= checkScalarExp)
    where checkArrayType typ@(_, Name _ (Just (Array _))) = Right typ
          checkArrayType alias@(env', Name _ (Just typ@(Name _ _))) =
            const alias <$> checkArrayType (env', typ)
          checkArrayType mismatch = Left $ typeError pos [Array Unit] (Right mismatch)
          checkLengthExp (ev, arrType) =
            verifyType ev TigerInt lengthExp
          checkScalarExp typ@(ev, arrType@((Name _ (Just (Array expected))))) =
            verifyType ev expected initExp
          checkScalarExp (ev, alias@((Name _ (Just arrTyp@(Name _ _))))) =
            checkScalarExp (ev, arrTyp)

checkRecord :: TypeEnv
               -> SourcePos
               -> TypeName
               -> [(Atom, Expression)]
               -> Either TypeError TExp
checkRecord env pos name fields =
    (lookupType env name pos) >>= checkRecordType >>= verifyTypesPresent >>= checkFieldsTypes
    where checkRecordType typ@(_, Name _ (Just (Record _))) = Right typ
          checkRecordType (env', alias@(Name _ (Just typ@(Name _ _)))) =
            mapType (const alias) $ checkRecordType (env', typ)
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
            mapType (const alias) $ verifyTypesPresent (env', typ)
          checkFieldsTypes typ@(_, Name _ (Just (Record typeFields))) =
            let exps = sortBy (comparing fst) fields
                types = snd <$> (sortBy (comparing fst ) typeFields)
            in T.RecordCreation pos typ name <$>
               (sequence $ fmap (\((n, exp), typ) -> ((,) n) <$> verifyType env typ exp)
                               (zip exps types))
          checkFieldsTypes (env', alias@(Name _ (Just typ@(Name _ _)))) =
            checkFieldsTypes (env', typ)
            -- TODO change mapType to work on TExp mapType (const alias) $ checkFieldsTypes (env', typ)


checkRecordAccess :: TypeEnv
                     -> SourcePos
                     -> LValue
                     -> Atom
                     -> Either TypeError (TypeInfo, T.LValue)
checkRecordAccess env pos lval@(Id record) field =
    flipTuple <$> (T.RecordAccess <$> (checkLVal env lval) <*> (pure field)) <*>
      (lookupValue env record pos >>= recordFields pos >>= recordFieldType pos field)
checkRecordAccess env pos lval@(RecordAccess record' field') field =
    flipTuple <$> (T.RecordAccess <$> (checkLVal env lval) <*> (pure field)) <*>
      (fst <$> checkRecordAccess env pos record' field' >>= recordFields pos >>= recordFieldType pos field)
checkRecordAccess env pos lval@(ArraySubscript array exp) field =
    flipTuple <$> (T.RecordAccess <$> (checkLVal env lval) <*> (pure field)) <*>
      (arrayElementType env pos array >>= recordFields pos >>= recordFieldType pos field)

recordFields :: SourcePos -> TypeInfo -> Either TypeError (TypeEnv, [(Atom, ProgramType)])
recordFields pos (env, (Record fields)) = Right (env, fields)
recordFields pos (env, (Name _ (Just (Record fields)))) = Right (env, fields)
recordFields pos (env, (Name _ (Just typ@(Name _ _)))) = recordFields pos (env, typ)
recordFields pos mismatch = Left $ typeError pos [Record []] (Right mismatch)

recordFieldType :: SourcePos -> Atom -> (TypeEnv, [(Atom, ProgramType)]) -> Either TypeError TypeInfo
recordFieldType pos field (env, fields) = (,) env <$>
    maybeToEither (lookup field fields) (undeclaredError Field pos field)

checkArraySubscript :: TypeEnv
                       -> SourcePos
                       -> LValue
                       -> Expression
                       -> Either TypeError (TypeInfo, T.LValue)
checkArraySubscript env pos arr@(ArraySubscript arr' subscript) exp = do
    tsubscript <- verifyType env TigerInt subscript
    ti@(env', _) <- arrayElementType (getTEnv tsubscript) pos arr
    texp <- verifyType env' TigerInt exp
    tarr <- checkLVal env arr'
    return $ (ti, T.ArraySubscript tarr texp)
checkArraySubscript env pos rec@(RecordAccess record field) exp =
    flipTuple <$>
      (T.ArraySubscript <$> checkLVal env rec <*> verifyType env TigerInt exp) <*>
      (fst <$> checkRecordAccess env pos record field >>= scalarType pos)
checkArraySubscript env pos lval@(Id name) exp =
    flipTuple <$>
      (T.ArraySubscript <$> checkLVal env lval <*> verifyType env TigerInt exp) <*>
      arrayElementType env pos lval

arrayElementType :: TypeEnv ->
                    SourcePos ->
                    LValue ->
                    Either TypeError TypeInfo
arrayElementType env pos lval = typeCheck env (LValExp pos lval) >>= scalarType pos . typeInfo

scalarType :: SourcePos -> TypeInfo -> Either TypeError TypeInfo
scalarType _ (env, Array typ) = Right (env, typ)
scalarType _ (env, Name _ (Just (Array typ))) = Right (env, typ)
scalarType pos (env, Name _ (Just typ@(Name _ _))) = scalarType pos (env, typ)
scalarType pos mismatch = Left $ typeError pos [Array Unit] (Right mismatch)

declareType :: TypeEnv ->
               SourcePos ->
               TypeName ->
               Type ->
               Either TypeError (TypeInfo, T.Declaration)
declareType env pos name (TypeId typeName) =
    flipTuple T.TypeDec <$> (addTypeBinding name id <$> lookupType env typeName pos)
declareType env pos name (ArrayOf typeName) =
    flipTuple T.TypeDec <$> (addTypeBinding name Array <$> lookupType env typeName pos)
declareType env pos name (RecordOf fields) =
    flipTuple T.TypeDec <$>
      ((addTypeBinding name id) <$>
        ((,) env) <$>
        Record <$>
        recordTypeFields)
    where recordTypeFields = sequence $
                            (\(fieldName, typeName) ->
                              ((,) fieldName) . snd <$>
                               (lookupType env typeName pos)) <$>
                            fields

addTypeBinding :: TypeName ->
                  (ProgramType -> ProgramType) ->
                  TypeInfo ->
                  TypeInfo
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
                   -> Either TypeError (TypeInfo, T.Declaration)
declareVariable e pos name Nothing exp = do
    texp <- typeCheck e exp
    let ti = addVarBinding name (typeInfo texp)
    return (ti, T.VarDec name texp)
declareVariable e pos name (Just typName) exp = do
    ti <- lookupType e typName pos
    texp <- verifyType' exp ti
    let ti = addVarBinding name (typeInfo texp)
    return (ti, T.VarDec name texp)
  where verifyType' exp (e', typ) = verifyType e' typ exp

addVarBinding :: Atom -> TypeInfo -> TypeInfo
addVarBinding name (env, typ) =
    let
      (symbol, tbl) = Sym.put name (sym env)
    in
      ( env { vEnv = Env.addBinding (symbol, typ) (vEnv env)
            , sym = tbl
            }
      , typ
      )

addVarScope :: TypeEnv -> [(Atom, ProgramType)] -> TypeEnv
addVarScope env typs =
    let
      initial = Sym.put "int" (sym env)
      atomsAndTables = scanr (\(name, _) (_, tbl')-> Sym.put name tbl') initial typs
      table' = snd $ head atomsAndTables
      atomsAndTypes = zip (fst <$> atomsAndTables) (snd <$> typs)
    in env { vEnv = Env.pushScope atomsAndTypes (vEnv env)
           , sym = table'
           }

popVarScope :: TypeEnv -> TypeEnv
popVarScope env = env { vEnv = Env.popScope (vEnv env) }

declareFn :: TypeEnv
             -> SourcePos
             -> Atom
             -> [(Atom, TypeName)]
             -> Maybe TypeName
             -> Expression
             -> Either TypeError (TypeInfo, T.Declaration)
declareFn e pos name fields retTyp body =
    do
      args <- argTypes pos e fields
      (_, returnType) <- lookupReturnType e pos retTyp
      let fnTyp = Function (fmap snd args) returnType
      let ti = (flip addVarScope $ args) $ fst $ addVarBinding name (e, fnTyp)
      tbody <- verifyType ti returnType body
      return $ ((ti, fnTyp), T.FnDec name (fmap fst fields) tbody)
    where lookupReturnType e pos (Just retTyp) = lookupType e retTyp pos
          lookupReturnType e _ Nothing = Right (e, Unit)

argTypes :: SourcePos -> TypeEnv -> [(Atom, TypeName)] -> Either TypeError [(Atom, ProgramType)]
argTypes pos e fields =
    (zip $ fmap fst fields) <$>
    fmap snd <$>
    mapM (\(_, typName) -> lookupType e typName pos) fields

checkVariableAssignment :: TypeEnv ->
                           SourcePos ->
                           Atom ->
                           Expression ->
                           Either TypeError TExp
checkVariableAssignment e pos var exp = do
      ti <- lookupValue e var pos
      texp <- verifyType e (snd ti) exp
      return $ T.Assignment pos ti (T.Id var) texp

checkArrayAssignment :: TypeEnv ->
                        SourcePos ->
                        LValue ->
                        Expression ->
                        Expression ->
                        Either TypeError TExp
checkArrayAssignment e pos arr sub exp = do
      (ti, (T.ArraySubscript lval tsub)) <- checkArraySubscript e pos arr sub
      tval <- checkAssignment exp ti
      return $ T.Assignment pos (fst ti, Unit) lval tval

checkRecordAssignment :: TypeEnv ->
                         SourcePos ->
                         LValue ->
                         Atom ->
                         Expression ->
                         Either TypeError TExp
checkRecordAssignment e pos rec field exp =
      fst <$> checkRecordAccess e pos rec field >>= checkAssignment exp

checkAssignment :: Expression ->
                   TypeInfo ->
                   Either TypeError TExp
checkAssignment exp (e', expected) = verifyType e' expected exp

checkFunctionCall :: TypeEnv ->
                     SourcePos ->
                     Atom ->
                     [Expression] ->
                     Either TypeError TExp
checkFunctionCall e pos fn args = do
    retType <- lookupValue e fn pos >>=
               verifyIsFunction >>=
               returnType
    tExps <- verifyNumArgs retType >>=
             verifyArgTypes
    return $ T.FunctionCall pos retType fn tExps
  where verifyIsFunction res@(_, Function _ _) = Right res
        verifyIsFunction mismatch = Left $ typeError pos [Function [] Unit] (Right mismatch)
        verifyNumArgs res@(_, Function params _)
          | length params == length args = Right res
          | otherwise = Left $ "Incorrect number of arguments: expected " ++
                        show (length params) ++
                        " given " ++
                        show (length args) ++
                        " at " ++
                        show pos
        verifyArgTypes res@(_, Function params _) =
          sequence $ fmap (uncurry $ verifyType e) (zip params args)
        returnType (e, Function _ typ) = return (e, typ)

checkIfThenElse :: TypeEnv ->
                   SourcePos ->
                   Expression ->
                   Expression ->
                   Expression ->
                   Either TypeError TExp
checkIfThenElse e pos bool truthy falsey = do
      bool' <- verifyType e TigerInt bool
      truthy' <- typeCheck e truthy
      falsey' <- verifyType e (getType truthy') falsey
      return $ T.IfThenElse pos (typeInfo truthy') bool' truthy' falsey'

checkIfThen :: TypeEnv ->
               SourcePos ->
               Expression ->
               Expression ->
               Either TypeError TExp
checkIfThen e pos bool truthy =
    verifyType e TigerInt bool >>
    verifyType e Unit truthy

checkFor :: TypeEnv ->
            SourcePos ->
            Atom ->
            Expression ->
            Expression ->
            Expression ->
            Either TypeError TExp
checkFor e pos idx fm to body =
    T.For pos (e, Unit) idx <$>
    verifyType e TigerInt fm <*>
    verifyType e TigerInt to <*>
    verifyType (addVarScope e [(idx, TigerInt)]) Unit body

checkWhile :: TypeEnv ->
              SourcePos ->
              Expression ->
              Expression ->
              Either TypeError TExp
checkWhile e pos bool body =
    T.While pos (e, Unit) <$>
      verifyType e TigerInt bool <*>
      verifyType e Unit body

checkLet :: TypeEnv ->
            SourcePos ->
            [Declaration] ->
            [Expression] ->
            Either TypeError TExp
checkLet = undefined
-- checkLet e pos decs exps =
--     (\(e', typ) -> (unscope e', typ)) <$>
--     (uniqTypeDecs decs pos >>
--      uniqFnDecs decs pos >>
--       (foldM (\(e', _) dec -> typeCheck e' (DecExp pos dec)) (initializeLetScope decs $ scope e, Unit) decs >>=
--       (\(e', _) -> typeCheck e' (Sequence pos exps))))
--     where scope env = env { tEnv = Env.pushScope [] (tEnv env)
--                           , vEnv = Env.pushScope [] (vEnv env)
--                           }
--           unscope env = env { tEnv = Env.popScope (tEnv env)
--                             , vEnv = Env.popScope (vEnv env)
--                             }
-- 
-- checkListExps :: TypeEnv ->
--                  [Expression] ->
--                  Either [TExp]
-- checkListExps _ [] = []
-- checkListExps e (exp:exps) =
--     let
--       texp = typeCheck e exp
--     in
--       case texp of
--         Right texp' -> Right $ texp':(checkListExps (getTEnv <$> texp') exps)
--         err@(Left _) -> err

uniqTypeDecs :: [Declaration] -> SourcePos -> Either TypeError ()
uniqTypeDecs ds pos = uniqDecs Type isTypeDec typeId pos ds
      where isTypeDec (TypeDec _ _) = True
            isTypeDec _ = False
            typeId (TypeDec name _) = name

uniqFnDecs :: [Declaration] -> SourcePos -> Either TypeError ()
uniqFnDecs ds pos = uniqDecs Fn isFnDec fnId pos ds
      where isFnDec (FnDec _ _ _ _) = True
            isFnDec _ = False
            fnId (FnDec name _ _ _) = name

uniqDecs :: Declarable ->
            (Declaration -> Bool) ->
            (Declaration -> String) ->
            SourcePos ->
            [Declaration] ->
            Either TypeError ()
uniqDecs decType filterFn nameFn pos ds =
    let
      names = nameFn <$> filter filterFn ds
      duplicates = names \\ (nub names)
    in
      if duplicates == []
      then Right ()
      else Left $ multipleDeclaredError decType pos duplicates

initializeLetScope :: [Declaration] -> TypeEnv -> TypeEnv
initializeLetScope [] e = e
initializeLetScope ((VarDec _ _ _):ds) e  = initializeLetScope ds e
initializeLetScope ((TypeDec name (TypeId _)):ds) e = initializeLetScope ds e
initializeLetScope ((TypeDec name typ):ds) e =
    let
      (symbol, tbl) = Sym.put name (sym e)
    in
      initializeLetScope ds
                         (e { tEnv = Env.addBinding (symbol, Name symbol Nothing)
                                                    (tEnv e)
                            , sym = tbl
                            })
initializeLetScope ((FnDec name args typ _):ds) e =
    let
      findType n = lookupTypeVal e n tEnv
      (symbol, tbl) = Sym.put name (sym e)
      returnType = case typ >>= findType of
                     Just t -> t
                     Nothing -> Unit
      argTypes = mapM findType (snd <$> args)
    in
      case argTypes of
        Nothing -> e
        Just ts -> initializeLetScope ds
                                      (e { vEnv = Env.addBinding (symbol, Function ts returnType)
                                                                 (vEnv e)
                                         , sym = tbl
                                         })

verifyType :: TypeEnv
              -> ProgramType
              -> Expression
              -> Either TypeError TExp
verifyType env typ (Nil pos) = if isNullable typ
                             then Right $ T.Nil pos (env, typ)
                             else Left $ show typ ++ " cannot be inhabited by nil"
verifyType env typ exp =
    let
      texp = typeCheck env exp
    in
      case typeInfo <$> texp of
        res@(Right (env', act)) ->
          if act == typ
          then texp
          else Left $ typeError (position exp) [typ] res
        mismatch -> Left $ typeError (position exp) [typ] mismatch

lookupType :: TypeEnv ->
              TypeName ->
              SourcePos ->
              Either TypeError TypeInfo
lookupType env name pos =
    (maybeToEither ((,) env <$> (lookupTypeVal env name tEnv))
                              (undeclaredError Type pos name))

lookupValue :: TypeEnv ->
               Atom ->
               SourcePos ->
               Either TypeError TypeInfo
lookupValue env name pos =
    (maybeToEither ((,) env <$> (lookupTypeVal env name vEnv))
                              (undeclaredError Identifier pos name))

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing e = Left e
maybeToEither (Just t) _ = Right t

flipTuple :: a -> b -> (b, a)
flipTuple = flip (,)

undeclaredError :: Declarable -> SourcePos -> Atom -> TypeError
undeclaredError dec pos name =
    "Type Error! Undeclared " ++ (show dec) ++ ": " ++ name ++ " at " ++ show pos

multipleDeclaredError :: Declarable -> SourcePos -> [Atom] -> TypeError
multipleDeclaredError dec pos names =
       "Multiple declarations of the same " ++
        show dec ++
        ": " ++
        (mconcat $ intersperse ", " names) ++
        " at " ++
        show pos

lookupTypeVal :: Show a => TypeEnv -> String -> (TypeEnv -> Env.Environment a) -> Maybe a
lookupTypeVal env name getEnv = Sym.get name (sym env) >>= lookup (getEnv env)
  where lookup = flip Env.lookup

typeError :: SourcePos -> [ProgramType] -> Either TypeError TypeInfo -> TypeError
typeError pos expected (Right (_, actual)) =
    "Type Error! Expected " ++
    (mconcat $ intersperse " or " (show <$> expected)) ++
    " but got " ++
    (show actual) ++ 
    " at " ++
    (show pos)
typeError _ _ (Left err) = err
