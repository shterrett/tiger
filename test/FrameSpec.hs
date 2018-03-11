module FrameSpec where

import Test.Hspec
import qualified Symbol as Sym
import qualified Temporary as Tmp
import Frame

spec =
    describe "Frame" $ do
      it "creates a new frame with the formals allocated" $ do
        let tbl = Sym.newTable 0
        let args = [Escape, Escape]
        let (label, tbl') = Tmp.newLabel tbl

        let (tbl'', frame) = newX86Frame label args tbl'

        name frame `shouldBe` label
        formals frame `shouldBe` [InFrame 8, InFrame 12]
        locals frame `shouldBe` []
        x86_nextOffset frame `shouldBe` 16

      it "initializes a local variable in an existing frame" $ do
        let tbl = Sym.newTable 0
        let args = [Escape, Escape]
        let (label, tbl') = Tmp.newLabel tbl

        let (tbl'', frame) = newX86Frame label args tbl'

        let ((tbl''', frame'), local) = allocLocal Escape frame tbl''

        local `shouldBe` (InFrame 16)
        locals frame' `shouldBe` [InFrame 16]
        x86_nextOffset frame' `shouldBe` 20
