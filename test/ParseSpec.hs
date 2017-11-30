module ParseSpec where

import Data.Either (isLeft, lefts)
import Test.Hspec
import qualified Text.Parsec as Parsec (parse, ParseError, Parsec)
import Parse
import TigerTypes

spec :: Spec
spec = do
  describe "formatting error messages" $ do
    it "shows the line, column and error on a failed parse" $ do
      let errorPrefix = "(line 1, column 3):\nunexpected 'i'"
      (take (length errorPrefix)) <$> (lefts [parse "1id"]) `shouldBe` [errorPrefix]
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
    describe "parsing array accesses" $ do
      it "uses the square brackets to denote an array access" $ do
        Parsec.parse lvalueParser "" "array[x]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
      it "allows arbitrary space within the brackets" $ do
        Parsec.parse lvalueParser "" "array[ x]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
        Parsec.parse lvalueParser "" "array[x ]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
        Parsec.parse lvalueParser "" "array[ x ]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
        Parsec.parse lvalueParser "" "array[\nx]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
        Parsec.parse lvalueParser "" "array[x\n]" `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ Id "x"))
      it "does not allow empty brackets" $ do
        isLeft (Parsec.parse lvalueParser "" "array[]") `shouldBe` True
        isLeft (Parsec.parse lvalueParser "" "array[  ]") `shouldBe` True

    it "allows combinations" $ do
      Parsec.parse lvalueParser "" "array[x].field_1"
        `shouldBe` Right (RecordAccess (ArraySubscript (Id "array") (LValExp $ Id "x")) "field_1")
      Parsec.parse lvalueParser "" "array[x.field_1]"
        `shouldBe` Right (ArraySubscript (Id "array") (LValExp $ RecordAccess (Id "x") "field_1"))
      Parsec.parse lvalueParser "" "x.field_1[y]"
        `shouldBe` Right (ArraySubscript (RecordAccess (Id "x") "field_1") (LValExp $ Id "y"))
  describe "parsing nil" $ do
    it "parses nil as Nil" $ do
      Parsec.parse nilParser "" "nil" `shouldBe` Right ()
    it "fails when nil is the beginning of a longer token" $ do
      isLeft (Parsec.parse nilParser "" "nilish") `shouldBe` True
      isLeft (Parsec.parse nilParser "" "nil_ish") `shouldBe` True
  describe "parsing sequence of expressions" $ do
    it "returns a list of parsed expressions" $ do
      Parsec.parse sequenceParser "" "(var x := y; var z := x)"
        `shouldBe` Right ([(DecExp $ VarDec "x" Nothing (LValExp $ Id "y")),
                           (DecExp $ VarDec "z" Nothing (LValExp $ Id "x"))])
    it "allows spaces within the list" $ do
      Parsec.parse sequenceParser "" "( var x := y ; var z := x )"
        `shouldBe` Right ([(DecExp $ VarDec "x" Nothing (LValExp $ Id "y")),
                           (DecExp $ VarDec "z" Nothing (LValExp $ Id "x"))])
    it "allows empty parentheses" $ do
      Parsec.parse sequenceParser "" "()" `shouldBe` Right []
      Parsec.parse sequenceParser "" "( )" `shouldBe` Right []
  describe "parsing integers" $ do
    it "parses a sequence of integers as a single integer literal" $ do
      Parsec.parse intParser "" "1234" `shouldBe` Right 1234
    it "fails if followed immediately by an alphanum character or underscore" $ do
      isLeft (Parsec.parse intParser "" "1234a") `shouldBe` True
      isLeft (Parsec.parse intParser "" "1234_") `shouldBe` True
  describe "parsing strings" $ do
    it "parses a string literal" $ do
      Parsec.parse stringParser "" "\"hello, world\"" `shouldBe` Right "hello, world"
    it "handles escape sequences" $ do
      Parsec.parse stringParser "" "\"hello\nworld\"" `shouldBe` Right "hello\nworld"
      Parsec.parse stringParser "" "\"hello\tworld\"" `shouldBe` Right "hello\tworld"
      Parsec.parse stringParser "" "\"hello\^Iworld\"" `shouldBe` Right "hello\tworld"
      Parsec.parse stringParser "" "\"hello\\world\"" `shouldBe` Right "hello\\world"
      Parsec.parse stringParser "" "\"hello \\\"world\\\"\"" `shouldBe` Right "hello \"world\""
      Parsec.parse stringParser "" "\"hello\59world\"" `shouldBe` Right "hello;world"
      Parsec.parse stringParser "" "\"hello\
      \ world\"" `shouldBe` Right "hello world"
  describe "negating an expression" $ do
    it "negates integer literals" $ do
      Parsec.parse negationParser "" "-1234" `shouldBe` Right (IntLiteral 1234)
    it "negates other expressions" $ do
      Parsec.parse negationParser "" "-a" `shouldBe` Right (LValExp $ Id "a")
    it "does not allow space after the hyphen" $ do
      isLeft (Parsec.parse negationParser "" "- a") `shouldBe` True
  describe "function call" $ do
    it "calls a function with its arguments" $ do
      Parsec.parse funcParser "" "map(like, friends)"
        `shouldBe` Right ("map", [LValExp (Id "like"), LValExp (Id "friends")])
    it "calls a function without arguments" $ do
      Parsec.parse funcParser "" "fireTheMissles()"
        `shouldBe` Right ("fireTheMissles", [])
