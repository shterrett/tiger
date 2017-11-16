module ParseSpec where

import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Parsec as Parsec (parse, ParseError, Parsec)
import Parse
import TigerTypes

spec :: Spec
spec = do
  describe "formatting error messages" $ do
    it "shows the line, column and error on a failed parse" $ do
      parse "1id" `shouldBe` Left "(line 1, column 1):\nunexpected \"1\"\nexpecting \"/*\", \"type\", \"var\", \"function\" or letter"
  describe "parsing comments" $ do
    it "parses comments between /* and */" $ do
      Parsec.parse commentParser "" "/* this is a comment */" `shouldBe` Right " this is a comment "
  describe "parsing declarations" $ do
    describe "parsing type declarations" $ do
      let fieldParser = typeFieldParser lbrace rbrace
      it "parses type fields" $ do
        Parsec.parse fieldParser ""  "{field_1: value_1, field_2: value_2}" `shouldBe`
          Right [("field_1", "value_1"), ("field_2", "value_2")]
      it "is not space sensitive" $ do
        Parsec.parse fieldParser ""  "{ field_1: value_1, field_2: value_2 }" `shouldBe`
          Right [("field_1", "value_1"), ("field_2", "value_2")]
        Parsec.parse fieldParser ""  "{field_1:value_1,field_2:value_2}" `shouldBe`
          Right [("field_1", "value_1"), ("field_2", "value_2")]
      it "requires a field and value" $ do
        isLeft (Parsec.parse fieldParser ""  "{ field_1: , field_2: value_2 }") `shouldBe` True
        isLeft (Parsec.parse fieldParser ""  "{ value_1 , field_2: value_2 }") `shouldBe` True
      it "parses empty type fields" $ do
        Parsec.parse fieldParser "" "{}" `shouldBe` Right []
      it "parses type fields into a type" $ do
        Parsec.parse typeParser "" "{ field_1: value_1 }" `shouldBe` Right (RecordOf [("field_1", "value_1")])
      it "parses a type name into a type" $ do
        Parsec.parse typeParser "" "string" `shouldBe` Right (TypeId "string")
      it "parses an array type" $ do
        Parsec.parse typeParser "" "array of int" `shouldBe` Right (ArrayOf "int")
      it "parses a type declaration" $ do
        Parsec.parse declarationParser "" "type email = string"
          `shouldBe` Right (TypeDec "email" (TypeId "string"))
    describe "parsing variable declarations" $ do
      it "parses assignment with a type id" $ do
        Parsec.parse declarationParser "" "var x: string := y"
          `shouldBe` Right (VarDec "x" (Just "string") (LValExp $ Id "y"))
      it "parses assignment without a type id" $ do
        Parsec.parse declarationParser "" "var x := y" `shouldBe` Right (VarDec "x" Nothing (LValExp $ Id "y"))
    describe "parsing function declaration" $ do
      it "parses function declaration with return type" $ do
        Parsec.parse declarationParser "" "function test (x: string) : string = x"
          `shouldBe` Right (FnDec "test" [("x", "string")] (Just "string") (LValExp $ Id "x"))
      it "parses function declaration without return type" $ do
        Parsec.parse declarationParser "" "function test (x: string) = x"
          `shouldBe` Right (FnDec "test" [("x", "string")] Nothing (LValExp $ Id "x"))
  describe "parsing LValues" $ do
    describe "parsing identifiers" $ do
      it "accepts a string composed of letters, numbers, and underscores" $ do
        Parsec.parse lvalueParser "" "valid_id" `shouldBe` Right (Id "valid_id")
        Parsec.parse lvalueParser "" "validId" `shouldBe` Right (Id "validId")
        Parsec.parse lvalueParser "" "id_123" `shouldBe` Right (Id "id_123")
        Parsec.parse lvalueParser "" "id123" `shouldBe` Right (Id "id123")
      it "rejects a string that starts with a number" $ do
        isLeft (Parsec.parse lvalueParser "" "1id") `shouldBe` True
        isLeft (Parsec.parse lvalueParser "" "1_id") `shouldBe` True
      it "rejects a string that starts with an underscore" $ do
        isLeft (Parsec.parse lvalueParser "" "_id") `shouldBe` True
    describe "parsing record accesses" $ do
      it "uses the dot to separate the record and the field" $ do
        Parsec.parse lvalueParser "" "record.field" `shouldBe` Right (RecordAccess (Id "record") "field")
      it "does not allow spaces around the dot" $ do
        isLeft (Parsec.parse lvalueParser "" "record. field") `shouldBe` True
        Parsec.parse lvalueParser "" "record .field"`shouldBe` Right (Id "record")
        Parsec.parse lvalueParser "" "record . field" `shouldBe` Right (Id "record")
      it "allows chained record access" $ do
        Parsec.parse lvalueParser "" "record.field_1.field_2"
          `shouldBe` Right (RecordAccess (RecordAccess (Id "record") "field_1") "field_2")
