module BinopPrecedenceSpec where

import Test.Hspec
import Text.Parsec.Pos (newPos)
import qualified Text.Parsec as Parsec (parse)
import Parse
import TigerTypes hiding (position)
import BinopPrecedence

position col = newPos "" 1 col

spec :: Spec
spec = do
    describe "it maps parsed binary operations into the proper precedence tree" $ do
      it "has no effect on a single binary operation" $ do
        setPrecedence <$> Parsec.parse binopParser "" "3 + 5"
          `shouldBe` Parsec.parse binopParser "" "3 + 5"
      it "has no effect on an operation in the correct precedence order" $ do
        setPrecedence <$> Parsec.parse binopParser "" "2 - 0 | 1"
          `shouldBe` Right (BinOp (position 6)
                                  Or
                                  (BinOp (position 2)
                                          Subtraction
                                          (IntLiteral (position 1) 2)
                                          (IntLiteral (position 5) 0))
                                  (IntLiteral (position 9) 1))
      it "rearranges one level of operator precedence" $ do
        setPrecedence <$> Parsec.parse binopParser "" "1 | 2 - 0"
          `shouldBe` Right (BinOp (position 2)
                                  Or
                                  (IntLiteral (position 1) 1)
                                  (BinOp (position 6)
                                          Subtraction
                                          (IntLiteral (position 5) 2)
                                          (IntLiteral (position 9) 0)))
      it "handles complex, nested expressions" $ do
        setPrecedence <$> Parsec.parse binopParser "" "1 | 2 - 3 * 4"
          `shouldBe` Right (BinOp (position 2)
                                   Or
                                   (IntLiteral (position 1) 1)
                                   (BinOp (position 6)
                                          Subtraction
                                          (IntLiteral (position 5) 2)
                                          (BinOp (position 8)
                                                 Multiplication
                                                 (IntLiteral (position 7) 3)
                                                 (IntLiteral (position 11) 4))))
