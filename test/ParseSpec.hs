module ParseSpec where

import Data.Either (isRight, isLeft, lefts)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import Test.Hspec
import qualified Text.Parsec as Parsec (parse, ParseError, Parsec)
import Text.Parsec.Pos (newPos)
import Parse
import AST hiding (position)

position col = newPos "" 1 col

spec :: Spec
spec = do
  describe "formatting error messages" $ do
    it "shows the line, column and error on a failed parse" $ do
      let errorPrefix = "(line 1, column 4):\nunexpected end"
      (take (length errorPrefix)) <$> (lefts [parse "1id"]) `shouldBe` [errorPrefix]
  describe "preparing program" $ do
    it "parses program without caring about comments" $ do
      parse "let\nvar /* variable declaration */ x /* named x */:int /* type integer */ := /* equals */ 5 /* five */\nin x end"
        `shouldBe` Right (Let (position 1)
                              [VarDec "x" (Just "int") (IntLiteral (newPos "" 2 79) 5)]
                              [LValExp (newPos "" 3 4) (Id "x")])
    it "strips multiline comments" $ do
      parse "let\nvar /* multiline\ncomment */ x /* named x */:int /* type integer */ := /* equals */ 5 /* five */\nin x end"
        `shouldBe` Right (Let (position 1)
                              [VarDec "x" (Just "int") (IntLiteral (newPos "" 3 61) 5)]
                              [LValExp (newPos "" 4 4) (Id "x")])
    it "strips leading whitespace left by comments" $ do
      parse "/* hello! */ let\nvar /* multiline\ncomment */ x /* named x */:int /* type integer */ := /* equals */ 5 /* five */\nin x end"
        `shouldBe` Right (Let (position 12)
                              [VarDec "x" (Just "int") (IntLiteral (newPos "" 3 61) 5)]
                              [LValExp (newPos "" 4 4) (Id "x")])
  describe "reserved words" $ do
    let reservedWords = [ "let"
                        , "in"
                        , "if"
                        , "then"
                        , "else"
                        , "end"
                        , "var"
                        , "type"
                        , "function"
                        , "array"
                        , "of"
                        , "nil"
                        , "while"
                        , "for"
                        , "to"
                        , "break"
                        ]
    it "parses reserved words" $ do
      Parsec.parse reservedWordsParser "" <$> reservedWords `shouldBe`
        Right <$> reservedWords
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
        Parsec.parse fieldParser ""  "{field_1:value_1 , field_2:value_2}" `shouldBe`
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
          `shouldBe` Right (VarDec "x" (Just "string") (LValExp (position 18) $ Id "y"))
        Parsec.parse declarationParser "" "var rec1: rectype1 := rectype2 {name=\"Name\", id=0}"
          `shouldBe` Right (VarDec "rec1"
                                   (Just "rectype1")
                                   (RecordCreation (position 23)
                                                   "rectype2"
                                                   [ ("name", StringLiteral (position 38) "Name")
                                                   , ("id", IntLiteral (position 49) 0)
                                                   ]))
      it "parses assignment without a type id" $ do
        Parsec.parse declarationParser "" "var x := y"
          `shouldBe` Right (VarDec "x" Nothing (LValExp (position 10) $ Id "y"))
    describe "parsing function declaration" $ do
      it "parses function declaration with return type" $ do
        Parsec.parse declarationParser "" "function test (x: string) : string = x"
          `shouldBe` Right (FnDec "test" [("x", "string")] (Just "string") (LValExp (position 38) $ Id "x"))
      it "parses function declaration without return type" $ do
        Parsec.parse declarationParser "" "function test (x: string) = x"
          `shouldBe` Right (FnDec "test" [("x", "string")] Nothing (LValExp (position 29) $ Id "x"))
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
        Parsec.parse lvalueParser "" "arr[x]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (position 5) $ Id "x"))
      it "allows arbitrary space within the brackets" $ do
        Parsec.parse lvalueParser "" "arr[ x]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (position 6) $ Id "x"))
        Parsec.parse lvalueParser "" "arr[x ]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (position 5) $ Id "x"))
        Parsec.parse lvalueParser "" "arr[ x ]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (position 6) $ Id "x"))
        Parsec.parse lvalueParser "" "arr[\nx]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (newPos "" 2 1) $ Id "x"))
        Parsec.parse lvalueParser "" "arr[x\n]"
          `shouldBe` Right (ArraySubscript (Id "arr")
                                           (LValExp (position 5) $ Id "x"))
      it "does not allow empty brackets" $ do
        isLeft (Parsec.parse lvalueParser "" "array[]") `shouldBe` True
        isLeft (Parsec.parse lvalueParser "" "array[  ]") `shouldBe` True

    it "allows combinations" $ do
      Parsec.parse lvalueParser "" "arr[x].field_1"
        `shouldBe` Right (RecordAccess (ArraySubscript (Id "arr") (LValExp (position 5) $ Id "x")) "field_1")
      Parsec.parse lvalueParser "" "arr[x.field_1]"
        `shouldBe` Right (ArraySubscript (Id "arr") (LValExp (position 5) $ RecordAccess (Id "x") "field_1"))
      Parsec.parse lvalueParser "" "x.field_1[y]"
        `shouldBe` Right (ArraySubscript (RecordAccess (Id "x") "field_1") (LValExp (position 11) $ Id "y"))
  describe "parsing nil" $ do
    it "parses nil as Nil" $ do
      Parsec.parse nilParser "" "nil" `shouldBe` Right ()
    it "fails when nil is the beginning of a longer token" $ do
      isLeft (Parsec.parse nilParser "" "nilish") `shouldBe` True
      isLeft (Parsec.parse nilParser "" "nil_ish") `shouldBe` True
  describe "parsing no value" $ do
    it "parses empty parentheses" $ do
      Parsec.parse noValueParser "" "()" `shouldBe` Right ()
      Parsec.parse noValueParser "" "( )" `shouldBe` Right ()
  describe "parsing sequence of expressions" $ do
    it "returns a list of parsed expressions" $ do
      Parsec.parse sequenceParser "" "x := y; z := x"
        `shouldBe` Right ([ (Assignment (position 1) (Id "x") (LValExp (position 6) $ Id "y"))
                          , (Assignment (position 9) (Id "z") (LValExp (position 14) $ Id "x"))
                          ])
    it "allows spaces within the list" $ do
      Parsec.parse sequenceParser "" "x := y ; z := x"
        `shouldBe` Right ([ (Assignment (position 1) (Id "x") (LValExp (position 6) $ Id "y"))
                          , (Assignment (position 10) (Id "z") (LValExp (position 15) $ Id "x"))
                          ])
    it "parses sequences of array access assignments with embedded arithmetic" $ do
      Parsec.parse sequenceParser "" "row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1"
        `shouldBe` Right ([ Assignment (position 1)
                                       (ArraySubscript (Id "row")
                                                       (LValExp (position 5) $ Id "r"))
                                       (IntLiteral (position 9) 1)
                          , Assignment (position 12)
                                       (ArraySubscript (Id "diag1")
                                                       (BinOp (position 19)
                                                              Addition
                                                              (LValExp (position 18) $ Id "r")
                                                              (LValExp (position 20) $ Id "c")))
                                       (IntLiteral (position 24) 1)
                          , Assignment (position 27)
                                       (ArraySubscript (Id "diag2")
                                                       (BinOp (position 36)
                                                              Subtraction
                                                              (BinOp (position 34)
                                                                      Addition
                                                                     (LValExp (position 33) $ Id "r")
                                                                     (IntLiteral (position 35) 7))
                                                              (LValExp (position 37) $ Id "c")))
                                       (IntLiteral (position 41) 1)
                         ])

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
      Parsec.parse negationParser "" "-1234" `shouldBe` Right (IntLiteral (position 2) 1234)
    it "negates other expressions" $ do
      Parsec.parse negationParser "" "-a" `shouldBe` Right (LValExp (position 2) $ Id "a")
    it "does not allow space after the hyphen" $ do
      isLeft (Parsec.parse negationParser "" "- a") `shouldBe` True
  describe "function call" $ do
    it "calls a function with its arguments" $ do
      Parsec.parse funcParser "" "map(like, friends)"
        `shouldBe` Right ("map", [ LValExp (position 5) (Id "like")
                                 , LValExp (position 11) (Id "friends")
                                 ])
    it "calls a function without arguments" $ do
      Parsec.parse funcParser "" "fireTheMissles()"
        `shouldBe` Right ("fireTheMissles", [])
  describe "Binary Operations" $ do
    describe "arithmetic" $ do
      it "parses basic arithmetic" $ do
        Parsec.parse binopParser "" "3 + 5"
          `shouldBe` Right (BinOp (position 2)
                            Addition
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 * 5"
          `shouldBe` Right (BinOp (position 2)
                            Multiplication
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 - 5"
          `shouldBe` Right (BinOp (position 2)
                            Subtraction
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 / 5"
          `shouldBe` Right (BinOp (position 2)
                            Division
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
      it "does not require space after the subtraction operator" $ do
        Parsec.parse binopParser "" "3-5"
          `shouldBe` Right (BinOp (position 2)
                            Subtraction
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 3) 5))
    describe "comparisons" $ do
      it "parses comparison operations" $ do
        Parsec.parse binopParser "" "3 > 5"
          `shouldBe` Right (BinOp (position 2)
                            GreaterThan
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 < 5"
          `shouldBe` Right (BinOp (position 2)
                            LessThan
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 >= 5"
          `shouldBe` Right (BinOp (position 2)
                            GreaterThanOrEqual
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 6) 5))
        Parsec.parse binopParser "" "3 <= 5"
          `shouldBe` Right (BinOp (position 2)
                            LessThanOrEqual
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 6) 5))
        Parsec.parse binopParser "" "3 = 5"
          `shouldBe` Right (BinOp (position 2)
                           Equality
                           (IntLiteral (position 1) 3)
                           (IntLiteral (position 5) 5))
        Parsec.parse binopParser "" "3 <> 5"
          `shouldBe` Right (BinOp (position 2)
                            NonEquality
                            (IntLiteral (position 1) 3)
                            (IntLiteral (position 6) 5))
    describe "boolean operations" $ do
      it "parses and and or" $ do
        Parsec.parse binopParser "" "x & y"
          `shouldBe` Right (BinOp (position 2)
                            And
                            (LValExp (position 1) $ Id "x")
                            (LValExp (position 5) $ Id "y"))
        Parsec.parse binopParser "" "x | y"
          `shouldBe` Right (BinOp (position 2)
                                  Or
                                  (LValExp (position 1) $ Id "x")
                                  (LValExp (position 5) $ Id "y"))
    describe "left associative" $ do
      it "parses multiple binary operations" $ do
        Parsec.parse binopParser "" "3 + 5 - 10"
          `shouldBe` Right (BinOp (position 6)
                                  Subtraction
                                  (BinOp (position 2)
                                          Addition
                                          (IntLiteral (position 1) 3)
                                          (IntLiteral (position 5) 5))
                                  (IntLiteral (position 9) 10))
    describe "partial match" $ do
      it "does not match an expression without an operator" $ do
        isLeft (Parsec.parse binopParser "" "5") `shouldBe` True
  describe "Record creation" $ do
    it "parses a record and its fields" $ do
      Parsec.parse recordCreateParser "" "Person { name = \"Houdini\", email = \"imakitty@example.com\", age = 3 }"
        `shouldBe` Right ( "Person"
                         , [ ("name", StringLiteral (position 17) "Houdini")
                           , ("email", StringLiteral (position 36) "imakitty@example.com")
                           , ("age", IntLiteral (position 66) 3)
                           ]
                         )
  describe "Array creation" $ do
    it "parses an array creation" $ do
      Parsec.parse arrayCreateParser "" "String [5] of \"hello\""
        `shouldBe` Right ( "String"
                         , IntLiteral (position 9) 5
                         , StringLiteral (position 15) "hello"
                         )
  describe "If Expressions" $ do
    it "parses an if-then-else" $ do
      Parsec.parse ifThenElseParser "" "if a\nthen b\nelse c"
        `shouldBe` Right ( LValExp (position 4) $ Id "a"
                         , LValExp (newPos "" 2 6) $ Id "b"
                         , LValExp (newPos "" 3 6) $ Id "c"
                         )
    it "parses an if-then" $ do
      Parsec.parse ifThenParser "" "if a\nthen b"
        `shouldBe` Right ( LValExp (position 4) $ Id "a"
                         , LValExp (newPos "" 2 6) $ Id "b"
                         )
  describe "Assignment" $ do
    it "parses := as assignment" $ do
      Parsec.parse assignmentParser "" "x := 5"
        `shouldBe` Right ( Id "x"
                         , IntLiteral (position 6) 5
                         )
  describe "while loops" $ do
    it "parses while ... do ... as a while loope" $ do
      Parsec.parse whileParser "" "while x do y"
        `shouldBe` Right ( LValExp (position 7) $ Id "x"
                         , LValExp (position 12) $ Id "y"
                         )
    it "handles newlines" $ do
      Parsec.parse whileParser "" "while x\ndo y"
        `shouldBe` Right ( LValExp (position 7) $ Id "x"
                         , LValExp (newPos "" 2 4) $ Id "y"
                         )
  describe "for loops" $ do
    it "parses for ... to ... do ..." $ do
      Parsec.parse forParser "" "for x := 5 to 10 do panic"
        `shouldBe` Right ( "x"
                         , IntLiteral (position 10) 5
                         , IntLiteral (position 15) 10
                         , LValExp (position 21) $ Id "panic"
                         )
  describe "break" $ do
    it "parses break when it stands alone" $ do
      Parsec.parse breakParser "" "break" `shouldBe` Right ()
      Parsec.parse breakParser "" " break " `shouldBe` Right ()
    it "does not parse break when it's part of another atom" $ do
      isLeft (Parsec.parse breakParser "" "break_down") `shouldBe` True
      isLeft (Parsec.parse breakParser "" "daybreak") `shouldBe` True
  describe "let - in" $ do
    it "parses a sequence of assignments and a sequence of expressions" $ do
      Parsec.parse letParser "" "let var x := 5\nin x\nend"
        `shouldBe` Right ([(VarDec "x" Nothing $  IntLiteral (position 14) 5)],
                          [LValExp (newPos "" 2 4) $ Id "x"])
  describe "group parser" $ do
    it "parses an expression surrounded by parentheses" $ do
      Parsec.parse groupParser "" "(3)"
        `shouldBe` Right (IntLiteral (position 2) 3)
  describe "expression" $ do
    it "parses all the things" $ do
      let syntaxErrors = ["test49.tig"]
      names <- filter (flip notElem $ syntaxErrors) <$>
               filter (isSuffixOf "tig") <$>
               getDirectoryContents "test/testcases"
      programs <- sequence $ readFile . (++) "test/testcases/" <$> names
      length programs `shouldNotBe` 0
      let results = fst <$> (filter (\(_, e) -> isLeft e) $ (zip names (parse <$> programs)))
      results `shouldBe` []
      errorPrograms <- sequence $ readFile . (++) "test/testcases/" <$> syntaxErrors
      let errorResults = fst <$> (filter (\(_, e) -> isRight e) $ (zip syntaxErrors (parse <$> errorPrograms)))
      errorResults `shouldBe` []
