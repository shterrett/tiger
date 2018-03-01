module TypingExampleSpec where

import Test.Hspec
import Data.Maybe (catMaybes)
import Data.Either (isRight)
import qualified Environment as Env
import qualified Symbol as Sym
import Parse (parse)
import Typing

checkTest :: String -> IO (Either TypeError (TypeEnv, ProgramType))
checkTest file = do
    let initialTypes =
          [ ("int", TigerInt)
          , ("string", TigerStr)
          , ("nil", Typing.Nil)
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

