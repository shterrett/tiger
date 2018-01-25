module TypingSpec where

import Test.Hspec
import Data.List (nub)
import qualified Environment as Env
import qualified Symbol as Sym
import TigerTypes (Expression(..), Operator(..))
import Text.Parsec.Pos (initialPos)
import Typing

spec = do
    let emptyEnv = ( Sym.newTable 0
                   , Env.fromList [] :: TypeEnv
                   )
    let dummyPos = initialPos ""
    describe "typeError" $ do
      it "formats an error message for a type mismatch" $ do
        typeError dummyPos [TigerInt] (Right TigerStr)
          `shouldBe` "Type Error! Expected Integer but got String at " ++ show dummyPos
      it "formats an error message with multiple accepted types" $ do
        typeError dummyPos [TigerInt, TigerStr] (Right Typing.Nil)
          `shouldBe` "Type Error! Expected Integer or String but got nil at " ++ show dummyPos
      it "passes through an error message that was encountered lower down" $ do
        typeError dummyPos [TigerInt] (Left "whoops!") `shouldBe` "whoops!"
    describe "typeCheck Nil" $ do
      it "types as Nil" $ do
        typeCheck emptyEnv (TigerTypes.Nil dummyPos) `shouldBe` (emptyEnv, Right Typing.Nil)
    describe "typeCheck Valueless" $ do
      it "types as Unit" $ do
        typeCheck emptyEnv (ValuelessExpression dummyPos (NoValue dummyPos))
          `shouldBe` (emptyEnv, Right Unit)
    describe "typeCheck NoValue" $ do
      it "types as Unit" $ do
        typeCheck emptyEnv (NoValue dummyPos) `shouldBe` (emptyEnv, Right Unit)
    describe "typeCheck IntLiteral" $ do
      it "types as TigerInt" $ do
        typeCheck emptyEnv (IntLiteral dummyPos 5) `shouldBe` (emptyEnv, Right TigerInt)
    describe "typeCheck StringLiteral" $ do
      it "types as TigerStr" $ do
        typeCheck emptyEnv (StringLiteral dummyPos "hello") `shouldBe` (emptyEnv, Right TigerStr)
    describe "typeCheck Negation" $ do
      it "types as TigerInt when the inner expression is an Int" $ do
        typeCheck emptyEnv (Negation dummyPos (IntLiteral dummyPos 5))
          `shouldBe` (emptyEnv, Right TigerInt)
      it "returns a mismatch error when the inner expression is not an int" $ do
        typeCheck emptyEnv (Negation dummyPos (StringLiteral dummyPos "hello"))
          `shouldBe` (emptyEnv, Left $ typeError dummyPos [TigerInt] (Right TigerStr))
    describe "typeCheck BinOp" $ do
      describe "arithmetic" $ do
        let operators = [Addition, Subtraction, Multiplication, Division]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Right TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right TigerStr)]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (TigerTypes.Nil dummyPos)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right Typing.Nil)]
      describe "comparison" $ do
        let operators = [Equality, NonEquality, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Right TigerInt)]
        it "types as TigerInt when all arguments are TigerStr" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hi")
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Right TigerInt)]
        it "returns a mismatch error favoring the left type when the types do not match" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] (Right TigerStr))]
        it "returns the left error if both types are wrong" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (NoValue dummyPos)
                                                 (TigerTypes.Nil dummyPos))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt, TigerStr] (Right Unit))]
      describe "and / or" $ do
        let operators = [And, Or]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Right TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right TigerStr)]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let typ op = typeCheck emptyEnv (BinOp dummyPos
                                                 op
                                                 (TigerTypes.Nil dummyPos)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [(emptyEnv, Left $ typeError dummyPos [TigerInt] $ Right Typing.Nil)]
