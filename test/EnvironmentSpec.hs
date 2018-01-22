module EnvironmentSpec where

import Prelude hiding (lookup)
import Test.Hspec
import Test.QuickCheck
import qualified Symbol
import Environment

spec = do
    let a = (Symbol.Symbol "a" 1)
    let b = (Symbol.Symbol "b" 2)
    let c = (Symbol.Symbol "c" 3)
    let d = (Symbol.Symbol "d" 4)
    let e = (Symbol.Symbol "e" 5)
    let f = (Symbol.Symbol "f" 6)
    describe "fromList" $ do
      it "constructs an environment with a single scope from a list of bindings" $ do
        let initial = [ (a, 1)
                      , (b, 2)
                      , (c, 3)
                      ] :: [(Symbol.Symbol, Int)]
        let env = fromList initial
        lookup a env `shouldBe` Just 1
        lookup b env `shouldBe` Just 2
        lookup c env `shouldBe` Just 3

    describe "scopes" $ do
      it "creates a new scope from a list of bindings with the old scope accessible" $ do
        let initial = fromList [ (a, 1)
                               , (b, 2)
                               , (c, 3)
                               ] :: Environment Int
        let updated = pushScope [ (d, 4)
                                , (e, 5)
                                , (f, 6)
                                ]
                                initial

        lookup a updated `shouldBe` Just 1
        lookup b updated `shouldBe` Just 2
        lookup c updated `shouldBe` Just 3
        lookup d updated `shouldBe` Just 4
        lookup e updated `shouldBe` Just 5
        lookup f updated `shouldBe` Just 6
      it "overrides a binding in a previous scope" $ do
        let initial = fromList [ (a, 1)
                               , (b, 2)
                               , (c, 3)
                               ] :: Environment Int
        let updated = pushScope [ (d, 4)
                                , (e, 5)
                                , (a, 6)
                                ]
                                initial
        lookup a updated `shouldBe` Just 6
        lookup b updated `shouldBe` Just 2
        lookup c updated `shouldBe` Just 3
        lookup d updated `shouldBe` Just 4
        lookup e updated `shouldBe` Just 5
      it "recovers the previous scope when the current scope is popped off" $ do
        let initial = fromList [ (a, 1)
                               , (b, 2)
                               , (c, 3)
                               ] :: Environment Int
        let updated = pushScope [ (d, 4)
                                , (e, 5)
                                , (a, 6)
                                ]
                                initial
        let previous = popScope updated
        lookup a previous `shouldBe` Just 1
        lookup b previous `shouldBe` Just 2
        lookup c previous `shouldBe` Just 3
        lookup d previous `shouldBe` Nothing
        lookup e previous `shouldBe` Nothing
      it "does not add a new scope when adding a new binding" $ do
        let initial = fromList [ (a, 1)
                               , (b, 2)
                               , (c, 3)
                               ] :: Environment Int
        let updated = pushScope [ (d, 4)
                                , (e, 5)
                                , (a, 6)
                                ]
                                initial
        let added = addBinding (f, 7) updated
        lookup a added `shouldBe` Just 6
        lookup b added `shouldBe` Just 2
        lookup c added `shouldBe` Just 3
        lookup d added `shouldBe` Just 4
        lookup e added `shouldBe` Just 5
        lookup f added `shouldBe` Just 7

        let previous = popScope added
        lookup a previous `shouldBe` Just 1
        lookup b previous `shouldBe` Just 2
        lookup c previous `shouldBe` Just 3
        lookup d previous `shouldBe` Nothing
        lookup e previous `shouldBe` Nothing
        lookup f previous `shouldBe` Nothing
