module StraightlineSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Straightline

spec :: Spec
spec = do
    describe "maxargs" $ do
      it "returns the number of arguments in a print statement" $ do
        let statement = PrintStm [ NumExp(5), NumExp(6), NumExp(7) ]
        maxargs statement `shouldBe` 3
      it "returns 0 if there is no print statement" $ do
        let statement = AssignStm "var" (NumExp 5)
        maxargs statement `shouldBe` 0
      it "returns the maximum number of args when multiple print statements exist" $ do
        let statement =
              CompoundStm
                (PrintStm [NumExp 5])
                (AssignStm "var"
                           (OpExp
                             (NumExp 5)
                             Plus
                             (EseqExp (PrintStm [NumExp 6, NumExp 7, NumExp 8]) (NumExp 9))))
        maxargs statement `shouldBe` 3
      it "returns the maximum number of args when print statements are nested" $ do
        let statement =
              PrintStm [ EseqExp (PrintStm [ NumExp 1, NumExp 2, NumExp 3 ])
                                 (NumExp 4)
                       , NumExp 5 ]
        maxargs statement `shouldBe` 3

    describe "interpExp" $ do
      it "evaluates a simple lookup" $ do
        let env = (Map.singleton "a" [3], [])
            statement = IdExp "a"
        interpExp env statement `shouldBe` (env, 3)
      it "evaluates a number literal" $ do
        let env = (Map.empty, [])
            statement = NumExp 5
        interpExp env statement `shouldBe` (env, 5)
      it "evaluates an arithmatic operation" $ do
        let env = (Map.empty, [])
            statement = OpExp (NumExp 5) Plus (NumExp 8)
        interpExp env statement `shouldBe` (env, 13)
      it "evaluates an expression sequence" $ do
        let env = (Map.empty, [])
            statement = EseqExp (AssignStm "a" (NumExp 5)) (OpExp (NumExp 7) Minus (IdExp "a"))
        interpExp env statement `shouldBe` ((Map.singleton "a" [5], []), 2)

    describe "interpStm" $ do
      it "evaluates an assign statement" $ do
        let env = (Map.empty, [])
            statement = AssignStm "a" (NumExp 5)
        interpStm env statement `shouldBe` (Map.singleton "a" [5], [])
      it "evaluates a compound statement, threading the state through" $ do
        let env = (Map.empty, [])
            statement = CompoundStm (AssignStm "a" (NumExp 5))
                                    (AssignStm "b" (NumExp 8))
            expectedStore = Map.fromList [("a", [5]), ("b", [8])]
        interpStm env statement `shouldBe` (expectedStore, [])
      it "evaluates a print statement in order" $ do
        let env = (Map.empty, [])
            statement = PrintStm [NumExp 5, NumExp 6, NumExp 7]
        interpStm env statement `shouldBe` (Map.empty, [5, 6, 7])

    describe "compound statements and expressions" $ do
      it "returns the most recently assigned value on lookup" $ do
        let env = (Map.empty, [])
            statement = EseqExp (CompoundStm (AssignStm "a" (NumExp 5))
                                             (AssignStm "a" (NumExp 9)))
                                (IdExp "a")
        let (_, val) = interpExp env statement
        val `shouldBe` 9
      it "threads the updated environments through a print statement" $ do
        let env = (Map.empty, [])
            statement =
              PrintStm [
                EseqExp (AssignStm "a" (NumExp 5))
                        (IdExp "a")
              , EseqExp (AssignStm "a" (OpExp (IdExp "a") Plus (NumExp 6)))
                        (IdExp "a")
              , EseqExp (AssignStm "a" (OpExp (IdExp "a") Minus (NumExp 2)))
                        (IdExp "a")
              ]
        let (_, output) = interpStm env statement
        output `shouldBe` [5, 11, 9]
      it "prints nested print statement output in order" $ do
        let env = (Map.empty, [])
            statement =
              PrintStm [
                EseqExp (AssignStm "a" (NumExp 5))
                        (IdExp "a")
              , EseqExp (CompoundStm (PrintStm [NumExp 1000])
                                     (AssignStm "a" (OpExp (IdExp "a") Plus (NumExp 6))))
                        (IdExp "a")
              , EseqExp (AssignStm "a" (OpExp (IdExp "a") Minus (NumExp 2)))
                        (IdExp "a")
              ]
        let (_, output) = interpStm env statement
        output `shouldBe` [5, 1000, 11, 9]
