module StraightlineSpec where

import Test.Hspec
import Straightline

spec :: Spec
spec =
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
