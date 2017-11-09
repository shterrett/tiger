module ParseSpec where

import Data.Either (isLeft)
import Test.Hspec
import Parse (parse)
import TigerTypes

spec :: Spec
spec = do
  describe "formatting error messages" $ do
    it "shows the line, column and error on a failed parse" $ do
      parse "1id" `shouldBe` Left "(line 1, column 1):\nunexpected \"1\"\nexpecting letter"
  describe "parsing identifiers" $ do
    let parsedId = Right . LValExp . Id
    it "accepts a string composed of letters, numbers, and underscores" $ do
      parse "valid_id" `shouldBe` parsedId "valid_id"
      parse "validId" `shouldBe` parsedId "validId"
      parse "id_123" `shouldBe` parsedId "id_123"
      parse "id123" `shouldBe` parsedId "id123"
    it "rejects a string that starts with a number" $ do
      isLeft (parse "1id") `shouldBe` True
      isLeft (parse "1_id") `shouldBe` True
    it "rejects a string that starts with an underscore" $ do
      isLeft (parse "_id") `shouldBe` True
