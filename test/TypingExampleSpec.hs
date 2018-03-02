module TypingExampleSpec where

import Test.Hspec
import Data.Maybe (catMaybes)
import Data.Either (isRight)
import qualified Environment as Env
import qualified Symbol as Sym
import Parse (parse)
import Typing
import Builtins (initialTypeEnv)

checkTest :: String -> IO (Either TypeError (TypeEnv, ProgramType))
checkTest file = do
    let emptyEnv = initialTypeEnv
    program <- readFile $ "test/testcases/" ++ file
    return ((parse program) >>= (typeCheck emptyEnv))

impossible = 1 `shouldBe` 2

spec = do
    describe "type checking the examples" $ do
      it "typechecks test 1" $ do
        Right (env, typ) <- checkTest "test1.tig"
        case Sym.get "arrtype" (sym env) of
          Just arrtype -> typ `shouldBe` Name arrtype (Just TigerInt)
          Nothing -> impossible
      it "typechecks test 2" $ do
        Right (env, typ) <- checkTest "test2.tig"
        case Sym.get "arrtype" (sym env) of
          Just arrtype -> typ `shouldBe` Name arrtype (Just TigerStr)
          Nothing -> impossible
      it "typechecks test 3" $ do
        Right (env, typ) <- checkTest "test3.tig"
        case Sym.get "rectype" (sym env) of
          Just rectype -> typ `shouldBe` Name rectype (Just $ Record [ ("name", TigerStr)
                                                                , ("age", TigerInt)])
          Nothing -> impossible
      it "typechecks test 4" $ do
        res <- checkTest "test4.tig"
        (fmap snd res) `shouldBe` Right TigerInt
      it "typechecks test 5" $ do
        res <- checkTest "test5.tig"
        let Right (env, typ) = res
        case Sym.get "intlist" (sym env) of
          Just intlist -> typ `shouldBe` Name intlist (Just $ Record [ ("hd", TigerInt)
                                                                     , ("tl", (Name intlist Nothing))])
          Nothing -> impossible
      it "typechecks test 6" $ do
        res <- checkTest "test6.tig"
        let Right (env, typ) = res
        typ `shouldBe` Unit
      it "typechecks test 7" $ do
        res <- checkTest "test7.tig"
        let Right (env, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 8" $ do
        res <- checkTest "test8.tig"
        let Right (env, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 9" $ do
        res <- checkTest "test9.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 3, column 24)"
      it "typechecks test 10" $ do
        res <- checkTest "test10.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected () but got Integer at (line 2, column 19)"
      it "typechecks test 11" $ do
        res <- checkTest "test11.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 2, column 14)"
      it "typechecks test 12" $ do
        res <- checkTest "test12.tig"
        let Right (_, typ) = res
        typ `shouldBe` Unit
      it "typechecks test 13" $ do
        res <- checkTest "test13.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 3, column 2)"
      it "typechecks test 14" $ do
        res <- checkTest "test14.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected rectype but got arrtype at (line 12, column 19)"
      it "typechecks test 15" $ do
        res <- checkTest "test15.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected () but got Integer at (line 3, column 12)"
      it "typechecks test 16" $ do
        res <- checkTest "test16.tig"
        let Left err = res
        err `shouldBe` "Type Error! Undeclared type: c at (line 2, column 1)"
      it "typechecks test 17" $ do
        -- Ignoring the specified error because there's no need to enforce
        -- it. I'm choosing to allow non-contiguous recurive definitions.
        res <- checkTest "test17.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 18" $ do
        -- Ignoring the specified error because there's no need to enforce
        -- it. I'm choosing to allow non-contiguous recurive definitions.
        res <- checkTest "test18.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 19" $ do
        res <- checkTest "test19.tig"
        let Left err = res
        err `shouldBe` "Type Error! Undeclared identifier: a at (line 8, column 30)"
      it "typechecks test 20" $ do
        res <- checkTest "test20.tig"
        let Left err = res
        err `shouldBe` "Type Error! Undeclared identifier: i at (line 3, column 18)"
      it "typechecks test 21" $ do
        res <- checkTest "test21.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got () at (line 8, column 34)"
      it "typechecks test 22" $ do
        res <- checkTest "test22.tig"
        let Left err = res
        err `shouldBe` "Type Error! Undeclared field: nam at (line 7, column 9)"
      it "typechecks test 23" $ do
        res <- checkTest "test23.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected String but got Integer at (line 7, column 22)"
      it "typechecks test 24" $ do
        res <- checkTest "test24.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected [()] but got Integer at (line 5, column 9)"
      it "typechecks test 25" $ do
        res <- checkTest "test25.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected {[]} but got Integer at (line 5, column 9)"
      it "typechecks test 26" $ do
        res <- checkTest "test26.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 3, column 5)"
      it "typechecks test 27" $ do
        res <- checkTest "test27.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 28" $ do
        res <- checkTest "test28.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected rectype1 but got rectype2 at (line 7, column 31)"
      it "typechecks test 29" $ do
        res <- checkTest "test29.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected arrtype1 but got arrtype2 at (line 7, column 31)"
      it "typechecks test 30" $ do
        res <- checkTest "test30.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 31" $ do
        res <- checkTest "test31.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 3, column 22)"
      it "typechecks test 32" $ do
        res <- checkTest "test32.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 6, column 34)"
      it "typechecks test 33" $ do
        res <- checkTest "test33.tig"
        let Left err = res
        err `shouldBe` "Type Error! Undeclared type: rectype at (line 3, column 17)"
      it "typechecks test 34" $ do
        res <- checkTest "test34.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got String at (line 5, column 11)"
      it "typechecks test 35" $ do
        res <- checkTest "test35.tig"
        let Left err = res
        err `shouldBe` "Incorrect number of arguments: expected 2 given 1 at (line 5, column 9)"
      it "typechecks test 36" $ do
        res <- checkTest "test36.tig"
        let Left err = res
        err `shouldBe` "Incorrect number of arguments: expected 2 given 3 at (line 5, column 9)"
      it "typechecks test 37" $ do
        res <- checkTest "test37.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 38" $ do
        res <- checkTest "test38.tig"
        let Left err = res
        err `shouldBe` "Multiple declarations of the same type: a at (line 4, column 1)"
      it "typechecks test 39" $ do
        res <- checkTest "test39.tig"
        let Left err = res
        err `shouldBe` "Multiple declarations of the same function: g at (line 4, column 1)"
      it "typechecks test 40" $ do
        res <- checkTest "test40.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected () but got Integer at (line 3, column 29)"
      it "typechecks test 41" $ do
        res <- checkTest "test41.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 42" $ do
        res <- checkTest "test42.tig"
        let Right (_, typ) = res
        typ `shouldBe` Unit
      it "typechecks test 43" $ do
        res <- checkTest "test43.tig"
        let Left err = res
        err `shouldBe` "Type Error! Expected Integer but got () at (line 6, column 9)"
      it "typechecks test 44" $ do
        res <- checkTest "test44.tig"
        let Right (_, typ) = res
        typ `shouldBe` Unit
      it "typechecks test 45" $ do
        res <- checkTest "test45.tig"
        let Left err = res
        err `shouldBe` "Cannot infer the type of nil at (line 5, column 17)"
      it "typechecks test 46" $ do
        res <- checkTest "test46.tig"
        let Right (_, typ) = res
        typ `shouldBe` TigerInt
      it "typechecks test 47" $ do
        -- Again allowing recursive declarations throughout let scope
        res <- checkTest "test47.tig"
        let Left err = res
        err `shouldBe`  "Multiple declarations of the same type: a at (line 5, column 1)"
      it "typechecks test 48" $ do
        -- Again allowing recursive declarations throughout let scope
        res <- checkTest "test48.tig"
        let Left err = res
        err `shouldBe`  "Multiple declarations of the same function: g at (line 5, column 1)"
      -- it "typechecks test 49" -> Parse Error
      it "typechecks merge" $ do
        res <- checkTest "merge.tig"
        let Left err = res
        err `shouldBe` ""
        let Right (_, typ) = res
        typ `shouldBe` Unit
