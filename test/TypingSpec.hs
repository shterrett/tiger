module TypingSpec where

import Test.Hspec
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Environment as Env
import qualified Symbol as Sym
import AST ( Expression(..)
           , Operator(..)
           , LValue(..)
           , Declaration(..)
           , Type(..)
           )
import Text.Parsec.Pos (initialPos, newPos)
import Typing
import Types ( TypeInfo
             , typeInfo
             , ProgramType(..)
             , TypeEnv(..)
             , TypeError
             , TExp
             , Declarable(..)
             , isNullable
             )

spec = do
    let typeCheck' env exp = typeInfo <$> typeCheck env exp
    let initialTypes =
          [ ("int", TigerInt)
          , ("string", TigerStr)
          ]
    let initialSymbolTable =
          foldr (\(s, _) tbl -> snd $ Sym.put s tbl)
                (Sym.newTable 0)
                initialTypes
    let initialEnv =
          Env.fromList . catMaybes $
          (fmap (\(s, t) -> (,) <$> Sym.get s initialSymbolTable <*> (Just t))
                initialTypes)
    let emptyEnv =
          TypeEnv { sym = initialSymbolTable
                  , tEnv = initialEnv
                  , vEnv = Env.fromList []
                  }
    let dummyPos = initialPos ""
    describe "typeError" $ do
      it "formats an error message for a type mismatch" $ do
        typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr))
          `shouldBe` "Type Error! Expected Integer but got String at " ++ show dummyPos
      it "formats an error message with multiple accepted types" $  do
        typeError dummyPos [TigerInt, TigerStr] (Right (emptyEnv, Array TigerInt))
          `shouldBe` "Type Error! Expected Integer or String but got [Integer] at " ++ show dummyPos
      it "passes through an error message that was encountered lower down" $ do
        typeError dummyPos [TigerInt] (Left "whoops!") `shouldBe` "whoops!"
    describe "typeCheck' Nil" $ do
      it "types as Nil" $ do
        typeCheck' emptyEnv (Nil dummyPos) `shouldBe` Left "Cannot infer the type of nil at (line 1, column 1)"
    describe "typeCheck' Valueless" $ do
      it "types as Unit" $ do
        typeCheck' emptyEnv (ValuelessExpression dummyPos (NoValue dummyPos))
          `shouldBe` Right (emptyEnv, Unit)
    describe "typeCheck' NoValue" $ do
      it "types as Unit" $ do
        typeCheck' emptyEnv (NoValue dummyPos) `shouldBe` Right (emptyEnv, Unit)
    describe "typeCheck' Break" $ do
      it "typechecks as Unit" $ do
        typeCheck' emptyEnv (Break dummyPos) `shouldBe` Right (emptyEnv, Unit)
    describe "typeCheck' IntLiteral" $ do
      it "types as TigerInt" $ do
        typeCheck' emptyEnv (IntLiteral dummyPos 5) `shouldBe` Right (emptyEnv, TigerInt)
    describe "typeCheck' StringLiteral" $ do
      it "types as TigerStr" $ do
        typeCheck' emptyEnv (StringLiteral dummyPos "hello") `shouldBe` Right (emptyEnv, TigerStr)
    describe "typeCheck' Negation" $ do
      it "types as TigerInt when the inner expression is an Int" $ do
        typeCheck' emptyEnv (Negation dummyPos (IntLiteral dummyPos 5))
          `shouldBe` Right (emptyEnv, TigerInt)
      it "returns a mismatch error when the inner expression is not an int" $ do
        typeCheck' emptyEnv (Negation dummyPos (StringLiteral dummyPos "hello"))
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck' BinOp" $ do
      describe "arithmetic" $ do
        let operators = [Addition, Subtraction, Multiplication, Division]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ (typeError dummyPos [TigerInt] $ Right (emptyEnv ,TigerStr))]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let pos2 = newPos "" 2 1
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (StringLiteral pos2 "world"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
      describe "comparison" $ do
        let operators = [LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "types as TigerInt when all arguments are TigerStr" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hi")
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error favoring the left type when the types do not match" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (StringLiteral dummyPos "bye"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr))]
        it "returns the left error if both types are wrong" $ do
          let (ints, table) = Sym.put "ints" (sym emptyEnv)
          let env = emptyEnv { tEnv = Env.addBinding (ints, (Array TigerInt)) (tEnv emptyEnv)
                             , sym = table
                             }
          let pos1 = newPos "" 2 1
          let typ op = typeCheck' env (BinOp dummyPos
                                            op
                                            (NoValue dummyPos)
                                            (ArrayCreation pos1
                                                            "ints"
                                                            (IntLiteral pos1 5)
                                                            (IntLiteral pos1 0)))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt, TigerStr] (Right (emptyEnv, Unit))]
      describe "equality" $ do
        let operators = [Equality, NonEquality]
        it "allows records to be compared" $ do
          let (person, table) = Sym.put "person" $ initialSymbolTable
          let personType = Name person $ Just (Record [("name", TigerStr)])
          let env = emptyEnv { sym = table
                            , tEnv  = (Env.pushScope [( person
                                                      , personType
                                                      )]
                                                      (tEnv emptyEnv))
                            }
          let p = RecordCreation dummyPos "person" [("name", (StringLiteral dummyPos "William"))]
          let typ op = typeCheck' env (BinOp dummyPos
                                            op
                                            p
                                            p)
          (nub $ typ <$> operators)
            `shouldBe` [Right (env, TigerInt)]
        it "allows array comparison" $ do
          let (ints, table) = Sym.put "ints" (sym emptyEnv)
          let env = emptyEnv { tEnv = Env.addBinding (ints, (Name ints (Just $ Array TigerInt))) (tEnv emptyEnv)
                             , sym = table
                             }
          let pos1 = newPos "" 2 1
          let array = (ArrayCreation pos1
                                     "ints"
                                     (IntLiteral pos1 5)
                                     (IntLiteral pos1 0))
          let typ op = typeCheck' env (BinOp dummyPos
                                            op
                                            array
                                            array)
          (nub $ typ <$> operators)
            `shouldBe` [Right (env, TigerInt)]
      describe "and / or" $ do
        let operators = [And, Or]
        it "types as TigerInt when all arguments are TigerInt" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 5)
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Right (emptyEnv, TigerInt)]
        it "returns a mismatch error when the left expression is not an int" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "hello")
                                                 (IntLiteral dummyPos 3))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error when the right expression is not an int" $ do
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (IntLiteral dummyPos 3)
                                                 (StringLiteral dummyPos "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
        it "returns a mismatch error for the left expression when the both are not ints" $ do
          let pos2 = newPos "" 2 1
          let typ op = typeCheck' emptyEnv (BinOp dummyPos
                                                 op
                                                 (StringLiteral dummyPos "goodbye")
                                                 (StringLiteral pos2 "hello"))
          (nub $ typ <$> operators)
            `shouldBe` [Left $ typeError dummyPos [TigerInt] $ Right (emptyEnv, TigerStr)]
    describe "typeCheck' Grouped" $ do
      it "types as the type of the grouped expression" $ do
        typeCheck' emptyEnv (Grouped dummyPos (IntLiteral dummyPos 5))
          `shouldBe` Right (emptyEnv, TigerInt)
      it "passes through an error" $ do
        typeCheck' emptyEnv (Grouped dummyPos
                                    (BinOp dummyPos
                                           Addition
                                           (StringLiteral dummyPos "hi")
                                           (IntLiteral dummyPos 5)))
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck' Sequence" $ do
      it "types as Unit for an empty list" $ do
        typeCheck' emptyEnv (Sequence dummyPos []) `shouldBe` Right (emptyEnv, Unit)
      it "type checks as the type of the last expression" $ do
        typeCheck' emptyEnv (Sequence dummyPos
                                     [ IntLiteral dummyPos 5
                                     , StringLiteral dummyPos "hi"
                                     ])
          `shouldBe` Right (emptyEnv, TigerStr)
      it "passes through an error" $ do
        typeCheck' emptyEnv (Sequence dummyPos
                                     [ IntLiteral dummyPos 5
                                     , (BinOp dummyPos
                                              Addition
                                              (StringLiteral dummyPos "hi")
                                              (IntLiteral dummyPos 5))
                                     ])
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
      it "propogates an error early in the sequence" $ do
        typeCheck' emptyEnv (Sequence dummyPos
                                     [ (BinOp dummyPos
                                              Addition
                                              (StringLiteral dummyPos "hi")
                                              (IntLiteral dummyPos 5))
                                     , IntLiteral dummyPos 5
                                     ])
          `shouldBe` Left (typeError dummyPos [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck' ArrayCreation" $ do
      let (words, table) = Sym.put "words" $ initialSymbolTable
      let wordsType = Name words $ Just (Array TigerStr)
      let env = emptyEnv { sym = table
                         , tEnv = (Env.pushScope [(words, wordsType)]
                                                 (tEnv emptyEnv))
                         }
      let lenPos = newPos "" 1 5
      let iniPos = newPos "" 1 10
      it ("type checks as the stated array type when it is declared properly, " ++
         "the first expression typechecks to a TigerInt, " ++
         "and the second has the same type as the declared type") $ do
        typeCheck' env (ArrayCreation dummyPos
                                     "words"
                                     (IntLiteral lenPos 5)
                                     (StringLiteral iniPos ""))
          `shouldBe` Right (env, Name words (Just $ Array TigerStr))
      it "returns an error if the first expression is not an int" $ do
        typeCheck' env (ArrayCreation dummyPos
                                     "words"
                                     (StringLiteral lenPos "hi")
                                     (StringLiteral iniPos ""))
          `shouldBe` Left (typeError lenPos [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the second expression does not match the declared type" $ do
        typeCheck' env (ArrayCreation dummyPos
                                     "words"
                                     (IntLiteral lenPos 5)
                                     (IntLiteral iniPos 0))
          `shouldBe` Left (typeError iniPos [TigerStr] (Right (env, TigerInt)))
      it "returns an error if the stated type is not an array type" $ do
        let (number, table') = Sym.put "number" table
        let env' = env { sym = table'
                       , tEnv = (Env.addBinding (number, (Name number $ Just TigerInt)) (tEnv env))
                       }
        typeCheck' env' (ArrayCreation dummyPos
                                      "number"
                                      (IntLiteral lenPos 5)
                                      (IntLiteral iniPos 0))
          `shouldBe` Left (typeError dummyPos [Array Unit] (Right (env, (Name number $ Just TigerInt))))
      it "returns an error if the stated type is not declared" $ do
        typeCheck' env (ArrayCreation dummyPos
                                     "number"
                                     (IntLiteral lenPos 5)
                                     (IntLiteral iniPos 0))
          `shouldBe` Left (undeclaredError Type dummyPos "number")
      it "typeCheck's if the array type is aliased" $ do
        let (nouns, table') = Sym.put "nouns" $ sym env
        let nounsType = Name nouns $ Just wordsType
        let env' = env { sym = table'
                       , tEnv = (Env.addBinding (nouns, nounsType) (tEnv env))
                       }
        typeCheck' env' (ArrayCreation dummyPos
                                      "nouns"
                                      (IntLiteral lenPos 5)
                                      (StringLiteral iniPos ""))
          `shouldBe` Right (env', Name nouns (Just $ wordsType))
    describe "typeCheck' RecordCreation" $ do
      let (person, table) = Sym.put "person" $ initialSymbolTable
      let personType = Name person $ Just (Record [("name", TigerStr), ("age", TigerInt)])
      let env = emptyEnv { sym = table
                         , tEnv  = (Env.pushScope [( person
                                                  , personType
                                                  )]
                                                  (tEnv emptyEnv))
                         }
      let f1Pos = newPos "" 1 5
      let f2Pos = newPos "" 1 10
      it ("type checks as the stated record when it is declared properly, " ++
          "and each field matches the stated type") $ do
        typeCheck' env (RecordCreation dummyPos
                                      "person"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
        `shouldBe` Right (env, personType)
      it "type checks when fields are in a different order" $ do
        typeCheck' env (RecordCreation dummyPos
                                      "person"
                                      [ ("age", IntLiteral f1Pos 21)
                                      , ("name", StringLiteral f2Pos "John")
                                      ])
        `shouldBe` Right (env, personType)
      it "returns an error if one of the fields does not match the declared type" $ do
        typeCheck' env (RecordCreation dummyPos
                                      "person"
                                      [ ("age", StringLiteral f1Pos "old")
                                      , ("name", StringLiteral f2Pos "John")
                                      ])
        `shouldBe` Left (typeError f1Pos [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the declared type is not a record type" $ do
        typeCheck' env (RecordCreation dummyPos
                                      "string"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
        `shouldBe` Left (typeError dummyPos [Record []] (Right (env, TigerStr)))
      it "returns an error if the type is not declared" $ do
        typeCheck' env (RecordCreation dummyPos
                                      "alien"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
          `shouldBe` Left (undeclaredError Type dummyPos "alien")
      it "typeCheck's if the record type is aliased" $ do
        let (human, table') = Sym.put "human" $ sym env
        let humanType = Name human $ Just personType
        let env' = env { sym = table'
                       , tEnv = Env.addBinding (human, humanType) (tEnv env)
                       }
        typeCheck' env' (RecordCreation dummyPos
                                      "human"
                                      [ ("name", StringLiteral f1Pos "John")
                                      , ("age", IntLiteral f2Pos 21)
                                      ])
          `shouldBe` Right (env', humanType)
    describe "typeCheck' LValueExp Id" $ do
      it "returns the type of the atom" $ do
        let (x, table) = Sym.put "x" initialSymbolTable
        let env = emptyEnv { sym = table
                           , vEnv = (Env.pushScope [(x, TigerInt)] (vEnv emptyEnv))
                           }
        typeCheck' env (LValExp dummyPos $ Id "x")
          `shouldBe` Right (env, TigerInt)
      it "returns an error if the atom has not yet been declared" $ do
        typeCheck' emptyEnv (LValExp dummyPos $ Id "x")
          `shouldBe` Left (undeclaredError Identifier dummyPos "x")
    describe "typeCheck' LValue RecordAccess" $ do
      it "returns the type of the field of the record" $ do
        let (person, table) = Sym.put "person" initialSymbolTable
        let personType = Name person $ Just (Record [("name", TigerStr), ("age", TigerInt)])
        let env = emptyEnv { sym = table
                           , vEnv = (Env.pushScope [( person
                                                   , personType
                                                   )]
                                                   (vEnv emptyEnv))
                           }
        typeCheck' env (LValExp dummyPos $ RecordAccess (Id "person") "name")
          `shouldBe` Right (env, TigerStr)
      it "returns an error if the record has not been declared" $ do
        typeCheck' emptyEnv (LValExp dummyPos $ RecordAccess (Id "person") "name")
          `shouldBe` Left (undeclaredError Identifier dummyPos "person")
      it "returns an error if the field is not a member of the record type" $ do
        let (person, table) = Sym.put "person" initialSymbolTable
        let personType = Name person $ Just (Record [("name", TigerStr), ("age", TigerInt)])
        let env = emptyEnv { sym = table
                           , vEnv = (Env.pushScope [( person
                                                   , personType
                                                   )]
                                                   (vEnv emptyEnv))
                           }
        typeCheck' env (LValExp dummyPos $ RecordAccess (Id "person") "birthday")
          `shouldBe` Left (undeclaredError Field dummyPos "birthday")
      it "handles chained record accesses" $ do
        let (person, table) = Sym.put "person" initialSymbolTable
        let (pet, table') = Sym.put "pet" table
        let petType = Name pet $ Just (Record [("breed", TigerStr)])
        let personType = Name person $ Just (Record [("name", TigerStr), ("pet", petType)])
        let env = emptyEnv { sym = table
                           , vEnv = (Env.pushScope [ ( person
                                                     , personType
                                                     )
                                                   , ( pet
                                                     , petType
                                                     )]
                                                   (vEnv emptyEnv))
                           }
        typeCheck' env (LValExp dummyPos $ RecordAccess (RecordAccess (Id "person") "pet")
                                                       "breed")
          `shouldBe` Right (env, TigerStr)
      it "handles aliased record types" $ do
        let (person, table) = Sym.put "person" initialSymbolTable
        let personType = Name person $ Just (Record [("name", TigerStr), ("age", TigerInt)])
        let (human, table') = Sym.put "human" table
        let humanType = Name human $ Just personType
        let env = emptyEnv { sym = table'
                           , vEnv = Env.pushScope [(human, humanType)]
                                                  (vEnv emptyEnv)
                           , tEnv = Env.pushScope [ (human, humanType)
                                                  , (person, personType)
                                                  ]
                                                  (tEnv emptyEnv)
                           }
        typeCheck' env (LValExp dummyPos $ RecordAccess (Id "human") "name")
          `shouldBe` Right (env, TigerStr)
    describe "typeCheck' LValue ArraySubscript" $ do
      let (words, table) = Sym.put "words" $ initialSymbolTable
      let arrayType = Name words $ Just (Array TigerStr)
      let env = emptyEnv { sym = table
                         , vEnv = (Env.pushScope [(words, arrayType)]
                                                 (vEnv emptyEnv))
                         }
      let subPos = newPos "" 1 5

      it "returns the type of the array elements" $ do
        typeCheck' env (LValExp dummyPos $ ArraySubscript (Id "words") (IntLiteral subPos 1))
          `shouldBe` Right (env, TigerStr)
      it "returns a type error if the subscript expression does not typecheck as int" $ do
        typeCheck' env (LValExp dummyPos $ ArraySubscript (Id "words") (StringLiteral subPos "1"))
          `shouldBe` Left (typeError subPos [TigerInt] (Right (env, TigerStr)))
      it "handles nested array access" $ do
        let (numbers, table') = Sym.put "numbers" $ sym env
        let numbersType = Name numbers $ Just (Array TigerInt)
        let env' = env { sym = table'
                       , vEnv = (Env.addBinding (numbers, numbersType) (vEnv env))
                       }
        let subPos2 = newPos "" 1 10
        typeCheck' env' (LValExp dummyPos
                        (ArraySubscript (Id "words")
                                        (LValExp subPos
                                                (ArraySubscript (Id "numbers")
                                                                (IntLiteral subPos2 0)))))

          `shouldBe` Right (env', TigerStr)
      it "handles chained array access" $ do
        let (matrix, table) = Sym.put "matrix" $ initialSymbolTable
        let matrixType = Name matrix $ Just (Array (Array TigerInt))
        let env = emptyEnv { sym = table
                           , vEnv = (Env.pushScope [(words, matrixType)]
                                                   (vEnv emptyEnv))
                           }
        let subPos = newPos "" 1 5
        let subPos2 = newPos "" 1 7
        typeCheck' env (LValExp dummyPos
                               (ArraySubscript (ArraySubscript (Id "matrix")
                                                               (IntLiteral subPos 2))
                                               (IntLiteral subPos 3)))
          `shouldBe` Right (env, TigerInt)
      it "handles aliased array types" $ do
        let (nouns, table') = Sym.put "nouns" $ sym env
        let nounsType = Name nouns $ Just arrayType
        let env = emptyEnv { sym = table'
                           , vEnv = Env.pushScope [(nouns, nounsType)] (vEnv emptyEnv)
                           , tEnv = Env.pushScope [ (nouns, nounsType)
                                                  , (words, arrayType)
                                                  ]
                                                  (tEnv emptyEnv)
                           }
        typeCheck' env (LValExp dummyPos $ ArraySubscript (Id "nouns") (IntLiteral subPos 1))
          `shouldBe` Right (env, TigerStr)
    describe "nested record and array access" $ do
      let (people, table) = Sym.put "people" initialSymbolTable
      let (person, table') = Sym.put "person" table
      let (names, table'') = Sym.put "names" table'
      let (name, table''') = Sym.put "name" table''
      let nameType = Name name $ Just (Record [ ("first", TigerStr)
                                              , ("last", TigerStr)
                                              ])
      let namesType = Name names $ Just (Array nameType)
      let personType = Name person $ Just (Record [ ("names", namesType)
                                                  , ("age", TigerInt)
                                                  ])
      let peopleType = Name people $ Just (Array personType)
      let env = emptyEnv { sym = table'''
                         , tEnv = Env.pushScope [ (people, peopleType)
                                                , (person, personType)
                                                , (names, namesType)
                                                , (name, nameType)
                                                ]
                                                (tEnv emptyEnv)
                         , vEnv = Env.pushScope [ (people, peopleType)
                                                , (person, personType)
                                                , (names, namesType)
                                                , (name, nameType)
                                                ]
                                                (vEnv emptyEnv)
                         }

      let pos1 = newPos "" 1 1

      it "returns the type of a record field from an array element" $ do
        typeCheck' env (LValExp dummyPos
                               (RecordAccess (ArraySubscript (Id "people")
                                                             (IntLiteral pos1 0))
                                             "age"))
          `shouldBe` Right (env, TigerInt)
      it "returns the type of an array element in a record field" $ do
        typeCheck' env (LValExp dummyPos
                               (ArraySubscript (RecordAccess (Id "person") "names")
                                               (IntLiteral pos1 0)))
          `shouldBe` Right (env, nameType)
      it "returns the type of an array element with index as record field" $ do
        typeCheck' env (LValExp dummyPos
                               (ArraySubscript (Id "people")
                                               (LValExp pos1
                                                        (RecordAccess (Id "person")
                                                                      "age"))))
          `shouldBe` Right (env, personType)


    describe "typeCheck' assignment" $ do
      describe "assigning to a variable" $ do
        let (x, table) = Sym.put "x" $ sym emptyEnv
        let env = emptyEnv { vEnv = Env.addBinding (x, TigerInt) (vEnv emptyEnv)
                           , sym = table
                           }
        let expPos = newPos "" 1 5

        it "returns the Unit type" $ do
          typeCheck' env (Assignment dummyPos (Id "x") (IntLiteral expPos 5))
            `shouldBe` Right (env, Unit)
        it "returns an error if the variable is not declared" $ do
          typeCheck' env (Assignment dummyPos (Id "y") (IntLiteral expPos 5))
            `shouldBe` Left (undeclaredError Identifier dummyPos "y")
        it "returns an error if the expression type does not match the variable type" $ do
          typeCheck' env (Assignment dummyPos (Id "x") (StringLiteral expPos "hi!"))
            `shouldBe` Left (typeError expPos [TigerInt] $ Right (env, TigerStr))
      describe "assigning to an array subscript" $ do
        let (numbers, table) = Sym.put "numbers" $ sym emptyEnv
        let numbersType = Name numbers $ Just (Array TigerInt)
        let env = emptyEnv { vEnv = Env.addBinding (numbers, numbersType) (vEnv emptyEnv)
                           , sym = table
                           }
        let subPos = newPos "" 1 2
        let expPos = newPos "" 1 5
        it "returns the Unit type" $ do
          typeCheck' env (Assignment dummyPos
                                    (ArraySubscript (Id "numbers")
                                                    (IntLiteral subPos 5))
                                    (IntLiteral expPos 10))
            `shouldBe` Right (env, Unit)
        it "returns an error if the array is not declared" $ do
          typeCheck' env (Assignment dummyPos
                                    (ArraySubscript (Id "words")
                                                    (IntLiteral subPos 5))
                                    (StringLiteral expPos "Hi!"))
            `shouldBe` Left (undeclaredError Identifier dummyPos "words")
        it "returns an error if the expression type does not match the array type" $ do
          typeCheck' env (Assignment dummyPos
                                    (ArraySubscript (Id "numbers")
                                                    (IntLiteral subPos 5))
                                    (StringLiteral expPos "Hi!"))
            `shouldBe` Left (typeError expPos [TigerInt] $ Right (env, TigerStr))
      describe "assigning to record access" $ do
        let (person, table) = Sym.put "person" $ sym emptyEnv
        let personType = Name person $ Just (Record [ ("first_name", TigerStr)
                                                    , ("age", TigerInt)
                                                    ])
        let env = emptyEnv { vEnv = Env.addBinding (person, personType) (vEnv emptyEnv)
                           , sym = table
                           }
        let expPos = newPos "" 1 5
        it "returns the Unit type" $ do
          typeCheck' env (Assignment dummyPos
                                    (RecordAccess (Id "person") "first_name")
                                    (StringLiteral expPos "Hugh"))
            `shouldBe` Right (env, Unit)
        it "returns an error if the record is not defined" $ do
          typeCheck' env (Assignment dummyPos
                                    (RecordAccess (Id "human") "first_name")
                                    (StringLiteral expPos "Hugh"))
            `shouldBe` Left (undeclaredError Identifier dummyPos "human")
        it "returns an error if the expression type does not match the field type" $ do
          typeCheck' env (Assignment dummyPos
                                    (RecordAccess (Id "person") "age")
                                    (StringLiteral expPos "Hugh"))
            `shouldBe` Left (typeError expPos [TigerInt] $ Right (env, TigerStr))
    describe "typeCheck' function call" $ do
      let (add, table) = Sym.put "add" $ sym emptyEnv
      let addType = Function [TigerInt, TigerInt] TigerInt
      let env = emptyEnv { vEnv = Env.addBinding (add, addType) (vEnv emptyEnv)
                          , sym = table
                          }
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 1 10
      it "returns the return type of the function" $ do
        let Right (_, typ) =
              typeCheck' env (FunctionCall dummyPos "add" [(IntLiteral pos1 5), (IntLiteral pos2 6)])
        typ `shouldBe` TigerInt
      it "fails if the function is not defined" $ do
        typeCheck' env (FunctionCall dummyPos "subtract" [(IntLiteral pos1 5), (IntLiteral pos2 6)])
          `shouldBe` Left (undeclaredError Identifier dummyPos "subtract")
      it "fails if the identifier type is not a function" $ do
        let (word, table') = Sym.put "word" $ sym emptyEnv
        let env' = env { vEnv = Env.addBinding (word, TigerStr) (vEnv emptyEnv)
                       , sym = table'
                       }
        typeCheck' env' (FunctionCall dummyPos "word" [(IntLiteral pos1 5), (IntLiteral pos2 6)])
          `shouldBe` Left (typeError dummyPos [Function [] Unit] $ Right (env, TigerStr))
      it "fails if the number of arguments is incorrect" $ do
        typeCheck' env (FunctionCall dummyPos "add" [(IntLiteral pos1 5)])
          `shouldBe` Left "Incorrect number of arguments: expected 2 given 1 at (line 1, column 1)"
      it "fails if the arguments do not match the types of the parameters" $ do
        typeCheck' env (FunctionCall dummyPos "add" [(IntLiteral pos1 5), (StringLiteral pos2 "Hi")])
          `shouldBe` Left (typeError pos2 [TigerInt] $ Right (env, TigerStr))
    describe "typeCheck' if-then-else" $ do
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 2 5
      let pos3 = newPos "" 3 5
      it "returns the type of both branch expressions" $ do
        typeCheck' emptyEnv (IfThenElse dummyPos
                                       (IntLiteral pos1 1)
                                       (StringLiteral pos2 "hi")
                                       (StringLiteral pos3 "bye"))
          `shouldBe` Right (emptyEnv, TigerStr)
      it "returns an error if the first expression is not an integer" $ do
        typeCheck' emptyEnv (IfThenElse dummyPos
                                       (StringLiteral pos1 "false")
                                       (StringLiteral pos2 "hi")
                                       (StringLiteral pos3 "bye"))
          `shouldBe` Left (typeError pos1 [TigerInt] (Right (emptyEnv, TigerStr)))
      it "returns an error if the two branch expressions have different types" $ do
        typeCheck' emptyEnv (IfThenElse dummyPos
                                       (IntLiteral pos1 1)
                                       (IntLiteral pos2 5)
                                       (StringLiteral pos3 "five"))
          `shouldBe` Left (typeError pos3 [TigerInt] (Right (emptyEnv, TigerStr)))
    describe "typeCheck' if-then" $ do
      let (print, table) = Sym.put "print" $ sym emptyEnv
      let printType = Function [TigerStr] Unit
      let env = emptyEnv { vEnv = Env.addBinding (print, printType) (vEnv emptyEnv)
                          , sym = table
                          }
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 2 5
      let pos3 = newPos "" 2 10
      it "returns the Unit type" $ do
        let Right (_, typ) = typeCheck' env (IfThen dummyPos
                                           (IntLiteral pos1 1)
                                           (FunctionCall pos2 "print" [StringLiteral pos3 "hi"]))
        typ `shouldBe` Unit
      it "returns an error if the first expression is not an integer" $ do
        typeCheck' env (IfThen dummyPos
                              (StringLiteral pos1 "false")
                              (FunctionCall pos2 "print" [StringLiteral pos3 "hi"]))
          `shouldBe` Left (typeError pos1 [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the second expression does not return unit" $ do
        typeCheck' env (IfThen dummyPos
                              (IntLiteral pos1 1)
                              (StringLiteral pos2 "hi"))
          `shouldBe` Left (typeError pos2 [Unit] (Right (env, TigerStr)))
    describe "typeCheck' for loop" $ do
      let (print, table) = Sym.put "print" $ sym emptyEnv
      let printType = Function [TigerInt] Unit
      let env = emptyEnv { vEnv = Env.addBinding (print, printType) (vEnv emptyEnv)
                          , sym = table
                          }
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 1 10
      let pos3 = newPos "" 3 5
      let pos4 = newPos "" 3 10
      it "returns the unit type" $ do
        let Right (env', typ) = typeCheck' env (For dummyPos
                                               "i"
                                               (IntLiteral pos1 0)
                                               (IntLiteral pos2 9)
                                               (FunctionCall pos3
                                                             "print"
                                                             [LValExp pos4 (Id "i")]))
        typ `shouldBe` Unit
        Sym.get "i" (sym env') `shouldBe` Nothing
      it "returns an error if the from expression is not integer" $ do
        typeCheck' env (For dummyPos
                           "i"
                           (StringLiteral pos1 "hi")
                           (IntLiteral pos2 9)
                           (FunctionCall pos3
                                         "print"
                                         [LValExp pos4 (Id "i")]))
          `shouldBe` Left (typeError pos1 [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the to expression is not integer" $ do
        typeCheck' env (For dummyPos
                           "i"
                           (IntLiteral pos1 0)
                           (StringLiteral pos2 "hi")
                           (FunctionCall pos3
                                         "print"
                                         [LValExp pos4 (Id "i")]))
          `shouldBe` Left (typeError pos2 [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the body returns a value" $ do
        typeCheck' env (For dummyPos
                           "i"
                           (IntLiteral pos1 0)
                           (IntLiteral pos2 9)
                           (StringLiteral pos3 "hi"))
          `shouldBe` Left (typeError pos3 [Unit] (Right (env, TigerStr)))
    describe "typeCheck' while loop" $ do
      let (print, table) = Sym.put "print" $ sym emptyEnv
      let printType = Function [TigerStr] Unit
      let env = emptyEnv { vEnv = Env.addBinding (print, printType) (vEnv emptyEnv)
                          , sym = table
                          }
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 2 5
      let pos3 = newPos "" 2 10
      it "returns the unit type" $ do
        let Right (_, typ) = typeCheck' env (While dummyPos
                                                  (IntLiteral pos1 1)
                                                  (FunctionCall pos2
                                                                "print"
                                                                [StringLiteral pos3 "wheeee!"]))
        typ `shouldBe` Unit
      it "returns an error if the while expression is not an integer" $ do
        typeCheck' env (While dummyPos
                             (StringLiteral pos1 "true")
                             (FunctionCall pos2
                                           "print"
                                           [StringLiteral pos3 "wheeee!"]))
          `shouldBe` Left (typeError pos1 [TigerInt] (Right (env, TigerStr)))
      it "returns an error if the body returns a value" $ do
        typeCheck' env (While dummyPos
                             (IntLiteral pos1 1)
                             (StringLiteral pos2 "wheeee!"))
          `shouldBe` Left (typeError pos2 [Unit] (Right (env, TigerStr)))
    describe "typeCheck' let-in" $ do
      let pos1 = newPos "" 1 5
      let pos2 = newPos "" 2 5
      let pos3 = newPos "" 3 5
      let pos4 = newPos "" 4 5
      let pos5 = newPos "" 5 5
      let pos6 = newPos "" 6 5
      let pos7 = newPos "" 7 5
      let pos8 = newPos "" 8 5
      it "returns the type returned by the body" $ do
        let Right (env, typ) =
              typeCheck' emptyEnv
                        (Let dummyPos
                             [ (TypeDec "numbers" (ArrayOf "int"))
                             , (VarDec "numbers"
                                       (Just "numbers")
                                       (ArrayCreation pos1
                                                     "numbers"
                                                     (IntLiteral pos2 5)
                                                     (IntLiteral pos3 10)))
                             , (FnDec "index"
                                     [ ("input", "numbers")
                                     , ("i", "int")
                                     ]
                                     (Just "int")
                                     (LValExp pos4
                                               (ArraySubscript (Id "input")
                                                               (LValExp pos5 (Id "i")))))
                             ]
                             [ (FunctionCall pos6
                                             "index"
                                             [ (LValExp pos7 (Id "numbers"))
                                             , (IntLiteral pos8 2)
                                             ])
                             ])
        typ `shouldBe` TigerInt
        (tEnv env) `shouldBe` (tEnv emptyEnv)
        (vEnv env) `shouldBe` (vEnv emptyEnv)
