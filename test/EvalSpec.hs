module EvalSpec where

import Test.Hspec
import Data.Maybe (catMaybes)
import Text.Parsec.Pos (initialPos, newPos)
import Eval ( eval)
import qualified Eval as E
import Typing
import Types ( TypeInfo
             , typeInfo
             , ProgramType(..)
             , TypeError
             , TExp(..)
             , Declarable(..)
             , isNullable
             )
import qualified Types as T
import qualified Symbol as Sym
import qualified Environment as Env

spec = do
    let initialTypes =
          [ ("int", TigerInt)
          , ("string", TigerStr)
          ]
    let initialSymbolTable =
          foldr (\(s, _) tbl -> snd $ Sym.put s tbl)
                (Sym.newTable 0)
                initialTypes
    let initialTypeEnv =
          Env.fromList . catMaybes $
          (fmap (\(s, t) -> (,) <$> Sym.get s initialSymbolTable <*> (Just t))
                initialTypes)
    let typeEnv =
          T.TypeEnv { T.sym = initialSymbolTable
                    , T.tEnv = initialTypeEnv
                    , T.vEnv = Env.fromList []
                    }
    let emptyEnv =
          E.EvalEnv { E.vEnv = Env.fromList []
                    , E.sym = initialSymbolTable
                    }
    let dummyPos = initialPos ""

    describe "evaluating literals" $ do
      it "evaluates an integer literal" $ do
        eval emptyEnv (IntLiteral dummyPos (typeEnv, TigerInt) 5)
          `shouldBe` Right (E.TigerInt 5)
      it "evaluates a string literal" $ do
        eval emptyEnv (StringLiteral dummyPos (typeEnv, TigerStr) "hello")
          `shouldBe` Right (E.TigerStr "hello")
