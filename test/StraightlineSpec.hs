module StraightlineSpec where

import Test.Hspec
import Straightline

spec :: Spec
spec =
    describe "maxargs" $ do
      it "returns the number of arguments in a print statement" $ do
        let statement = PrintStm [ NumExp(5), NumExp(6), NumExp(7) ]
        maxargs statement `shouldBe` 3


