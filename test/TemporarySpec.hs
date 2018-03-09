module TemporarySpec where

import Test.Hspec
import qualified Symbol as Sym
import qualified Temporary as Tmp

spec :: Spec
spec =
    describe "Temporary" $ do
      it "returns incrementally numbered temporary variables" $ do
        let tbl = Sym.newTable 0
        let (v1, tbl_1) = Tmp.newVar tbl
        let (v2, tbl_2) = Tmp.newVar tbl_1
        v1 == v2 `shouldBe` False
        show v1 `shouldBe` "tmp_v"
        show v2 `shouldBe` "tmp_v_1"
      it "returns incrementally numbered temporary labels" $ do
        let tbl = Sym.newTable 0
        let (l1, tbl_1) = Tmp.newLabel tbl
        let (l2, tbl_2) = Tmp.newLabel tbl_1

        l1 == l2 `shouldBe` False
        show l1 `shouldBe` "tmp_l"
        show l2 `shouldBe` "tmp_l_1"
