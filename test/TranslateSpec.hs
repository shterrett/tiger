module TranslateSpec where

import Test.Hspec
import qualified Symbol as Sym
import qualified Temporary as Tmp
import qualified Frame
import Translate

spec :: Spec
spec =
    describe "Translate" $ do
      it "returns a new Access that contains the frame and the level and an extra formal for the link" $ do
        let tbl = Sym.newTable 0
        let (label, tbl') = Tmp.newLabel tbl
        let (tbl'', level) = newLevel Outermost label [Frame.Escape] tbl'

        level `shouldBe`
          Nested Outermost (Frame.X86Frame { Frame.x86_name = label
                                           , Frame.x86_formals = [ Frame.InFrame 8
                                                                 , Frame.InFrame 12
                                                                 ]
                                           , Frame.x86_locals = []
                                           , Frame.x86_nextOffset = 16
                                           })

      it "does not return the first formal, and includes the level" $ do
        let tbl = Sym.newTable 0
        let (label, tbl') = Tmp.newLabel tbl
        let (tbl'', level) = newLevel Outermost label [Frame.Escape] tbl'

        formals level `shouldBe` [Access level (Frame.InFrame 12)]
      it "allocates locals and returns the level" $ do
        let tbl = Sym.newTable 0
        let (label, tbl') = Tmp.newLabel tbl
        let (tbl'', level) = newLevel Outermost label [Frame.Escape] tbl'
        let (Nested Outermost frame) = level

        let ((tbl''', level'), local) = allocLocal Frame.Escape level tbl''
        let ((tbl'''', frame'), local') = Frame.allocLocal Frame.Escape frame tbl''

        level' `shouldBe` Nested Outermost frame'
        local `shouldBe` Access (Nested Outermost frame') local'
      it "returns all allocated locals" $ do
        let tbl = Sym.newTable 0
        let (label, tbl') = Tmp.newLabel tbl
        let (tbl'', level) = newLevel Outermost label [Frame.Escape] tbl'
        let ((tbl''', level'), local) = allocLocal Frame.Escape level tbl''

        locals level' `shouldBe` [Access level' (Frame.InFrame 16)]
