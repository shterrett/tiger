module TypingSpec where

import Test.Hspec
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Environment as Env
import qualified Symbol as Sym
import TigerTypes (Expression(..), Operator(..))
import Text.Parsec.Pos (initialPos, newPos)
import Typing

spec = do
    let initialTypes =
          [ ("int", TigerInt)
          , ("string", TigerStr)
          , ("nil", Typing.Nil)
          ]
    let initialSymbolTable =
          foldr (\(s, _) tbl -> snd $ Sym.put s tbl)
                (Sym.newTable 0)
                initialTypes
    let initialEnv =
          Env.fromList . catMaybes $
          (fmap (\(s, t) -> (,) <$> Sym.get s initialSymbolTable <*> (Just t))
                initialTypes)
    let emptyEnv =
          ( initialSymbolTable
          , initialEnv
          ) :: TypeEnv
    let dummyPos = initialPos ""
    describe "typeError" $ do
      it "formats an error message for a type mismatch" $ do
        typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr))
          `shouldBe` "Type Error! Expected Integer but got String at " ++ show dummyPos
      it "formats an error message with multiple accepted types" $ do
        typeError dummyPos [TigerInt, TigerStr] (Right (emptyEnv, Typing.Nil))
          `shouldBe` "Type Error! Expected Integer or String but got nil at " ++ show dummyPos
      it "passes through an error message that was encountered lower down" $ do
        typeError dummyPos [TigerInt] (Left "whoops!") `shouldBe` "whoops!"
    describe "typeCheck Nil" $ do
      it "types as Nil" $ do
        typeCheck emptyEnv (TigerTypes.Nil dummyPos) `shouldBe` Right (emptyEnv, Typing.Nil)
    describe "typeCheck Valueless" $ do
      it "types as Unit" $ do
        typeCheck emptyEnv (ValuelessExpression dummyPos (NoValue dummyPos))
          `shouldBe` Right (emptyEnv, Unit)
    describe "typeCheck NoValue" $ do
      it "types as Unit" $ do
        typeCheck emptyEnv (NoValue dummyPos) `shouldBe` Right (emptyEnv, Unit)
    describe "typeCheck IntLiteral" $ do
      it "types as TigerInt" $ do
        typeCheck emptyEnv (IntLiteral dummyPos 5) `shouldBe` Right (emptyEnv, TigerInt)
    describe "typeCheck StringLiteral" $ do
      it "types as TigerStr" $ do
        typeCheck emptyEnv (StringLiteral dummyPos "hello") `shouldBe` Right (emptyEnv, TigerStr)
    describe "typeCheck Negation" $ do
      it "types as TigerInt when the inner expression is an Int" $ do
        typeCheck emptyEnv (Negation dummyPos (IntLiteral dummyPos 5))
          `shouldBe` Right (emptyEnv, TigerInt)
      it "returns a mismatch error when the inner expression is not an int" $ do
        typeCheck emptyEnv (Negation dummyPos (StringLiteral dummyPos "hello"))
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck BinOp" $ do
      describe "arithmetic" $ do
        let operators = [Addition, Subtraction, Multiplication, Division]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ (typeError dummyPos [TigerInt] $ Right (emptyEnv ,TigerStr))]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (TigerTypes.Nil dummyPos)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, Typing.Nil)]
      describe "comparison" $ do
        let operators = [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "types as TigerInt when all arguments are TigerStr" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hi")
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error favoring the left type when the types do not match" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr))]
        it "returns the left error if both types are wrong" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (NoValue dummyPos)
                                                 (TigerTypes.Nil dummyPos))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt, TigerStr] (Right (emptyEnv, Unit))]
      describe "and / or" $ do
        let operators = [And, Or]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (TigerTypes.Nil dummyPos)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, Typing.Nil)]
    describe "typeCheck Grouped" $ do
      it "types as the type of the grouped expression" $ do
        typeCheck emptyEnv (Grouped dummyPos (IntLiteral dummyPos 5))
          `shouldBe` Right (emptyEnv, TigerInt)
      it "passes through an error" $ do
        typeCheck emptyEnv (Grouped dummyPos
                                    (BinOp dummyPos
                                           Addition
                                           (StringLiteral dummyPos "hi")
                                           (IntLiteral dummyPos 5)))
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck Sequence" $ do
      it "types as Unit for an empty list" $ do
        typeCheck emptyEnv (Sequence dummyPos []) `shouldBe` Right (emptyEnv, Unit)
      it "type checks as the type of the last expression" $ do
        typeCheck emptyEnv (Sequence dummyPos
                                     [ IntLiteral dummyPos 5
                                     , StringLiteral dummyPos "hi"
                                     ])
          `shouldBe` Right (emptyEnv, TigerStr)
      it "passes through an error" $ do
        typeCheck emptyEnv (Sequence dummyPos
                                     [ IntLiteral dummyPos 5
                                     , (BinOp dummyPos
                                              Addition
                                              (StringLiteral dummyPos "hi")
                                              (IntLiteral dummyPos 5))
                                     ])
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
      it "propogates an error early in the sequence" $ do
        typeCheck emptyEnv (Sequence dummyPos
                                     [ (BinOp dummyPos
                                              Addition
                                              (StringLiteral dummyPos "hi")
                                              (IntLiteral dummyPos 5))
                                     , IntLiteral dummyPos 5
                                     ])
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck ArrayCreation" $ do
      let (words, table) = Sym.put "words" $ initialSymbolTable
      let env = ( table
                , Env.pushScope [(words, Name words $ Just (Array TigerStr))] initialEnv
                )
      let lenPos = newPos "" 1 5
      let iniPos = newPos "" 1 10
      it ("type checks as the stated array type when it is declared properly, " ++
         "the first expression typechecks to a TigerInt, " ++
         "and the second has the same type as the declared type") $ do
        typeCheck env (ArrayCreation dummyPos
                                     "words"
                                     (IntLiteral lenPos 5)
                                     (StringLiteral iniPos ""))
          `shouldBe` Right (env, Name words (Just $ Array TigerStr))
      it "returns an error if the first expression is not an int" $ do
        typeCheck env (ArrayCreation dummyPos
                                     "words"
                                     (StringLiteral lenPos "hi")
                                     (StringLiteral iniPos ""))
          `shouldBe` Left (typeError lenPos [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the second expression does not match the declared type" $ do
        typeCheck env (ArrayCreation dummyPos
                                     "words"
                                     (IntLiteral lenPos 5)
                                     (IntLiteral iniPos 0))
          `shouldBe` Left (typeError iniPos [TigerStr] (Right (env, TigerInt)))
      it "returns an error if the stated type is not an array type" $ do
        let (number, table') = Sym.put "number" table
        let env' = ( table'
                   , Env.addBinding (number, (Name number $ Just TigerInt)) (snd env)
                   )
        typeCheck env' (ArrayCreation dummyPos
                                      "number"
                                      (IntLiteral lenPos 5)
                                      (IntLiteral iniPos 0))
          `shouldBe` Left (typeError dummyPos [Array Unit] (Right (env, (Name number $ Just TigerInt))))
      it "returns an error if the stated type is not declared" $ do
        typeCheck env (ArrayCreation dummyPos
                                     "number"
                                     (IntLiteral lenPos 5)
                                     (IntLiteral iniPos 0))
          `shouldBe` Left (undeclaredType dummyPos "number")
    describe "typeCheck RecordCreation" $ do
      let (person, table) = Sym.put "person" $ initialSymbolTable
      let personType = Name person $ Just (Record [("name", TigerStr), ("age", TigerInt)])
      let env = ( table
                , Env.pushScope [( person
                                , personType
                                )]
                                (snd emptyEnv)
                )
      let f1Pos = newPos "" 1 5
      let f2Pos = newPos "" 1 10
      it ("type checks as the stated record when it is declared properly, " ++
          "and each field matches the stated type") $ do
        typeCheck env (RecordCreation dummyPos
                                      "person"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
        `shouldBe` Right (env, personType)
      it "type checks when fields are in a different order" $ do
        typeCheck env (RecordCreation dummyPos
                                      "person"
                                      [ ("age", IntLiteral f1Pos 21)
                                      , ("name", StringLiteral f2Pos "John")
                                      ])
        `shouldBe` Right (env, personType)
      it "returns an error if one of the fields does not match the declared type" $ do
        typeCheck env (RecordCreation dummyPos
                                      "person"
                                      [ ("age", StringLiteral f1Pos "old")
                                      , ("name", StringLiteral f2Pos "John")
                                      ])
        `shouldBe` Left (typeError f1Pos [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the declared type is not a record type" $ do
        typeCheck env (RecordCreation dummyPos
                                      "string"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
        `shouldBe` Left (typeError dummyPos [Record []] (Right (env, TigerStr)))
      it "returns an error if the type is not declared" $ do
        typeCheck env (RecordCreation dummyPos
                                      "alien"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
          `shouldBe` Left (undeclaredType dummyPos "alien")
