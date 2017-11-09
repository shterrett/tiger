module ParseSpec where

import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Parsec as Parsec (parse, ParseError, Parsec)
import Parse
import TigerTypes

testParse :: (Parsec.Parsec String () Expression) -> String -> Either Parsec.ParseError Expression
testParse parser = Parsec.parse parser ""

spec :: Spec
spec = do
  describe "formatting error messages" $ do
    it "shows the line, column and error on a failed parse" $ do
      parse "1id" `shouldBe` Left "(line 1, column 1):\nunexpected \"1\"\nexpecting letter"
  describe "parsing identifiers" $ do
    let parsedId = Right . LValExp . Id
    it "accepts a string composed of letters, numbers, and underscores" $ do
      testParse idParser "valid_id" `shouldBe` parsedId "valid_id"
      testParse idParser "validId" `shouldBe` parsedId "validId"
      testParse idParser "id_123" `shouldBe` parsedId "id_123"
      testParse idParser "id123" `shouldBe` parsedId "id123"
    it "rejects a string that starts with a number" $ do
      isLeft (testParse idParser "1id") `shouldBe` True
      isLeft (testParse idParser "1_id") `shouldBe` True
    it "rejects a string that starts with an underscore" $ do
      isLeft (testParse idParser "_id") `shouldBe` True
  describe "parsing comments" $ do
    it "parses comments between /* and */" $ do
      testParse commentParser "/* this is a comment */" `shouldBe` Right (Comment " this is a comment ")
