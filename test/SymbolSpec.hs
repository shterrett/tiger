module SymbolSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List (nub, foldl', sort)

import qualified Symbol

prop_equality_by_int :: String -> String -> Integer -> Integer -> Bool
prop_equality_by_int k_1 k_2 i_1 i_2 =
    let symbols = [Symbol.Symbol k i | k <- [k_1, k_2],
                                       i <- [i_1, i_2]]
        pairs = [(s_1, s_2) | s_1 <- symbols,
                              s_2 <- symbols]
    in
      ((uncurry (==)) <$> pairs)
        == ((\(Symbol.Symbol _ v_1, Symbol.Symbol _ v_2) -> v_1 == v_2) <$> pairs)

prop_stores_with_integer :: String -> Integer -> Bool
prop_stores_with_integer key i =
  (Symbol.get key . snd .Symbol.put key $ Symbol.newTable i)
    == Just (Symbol.Symbol key i)

prop_ordered_by_integer :: [String] -> [Integer] -> Bool
prop_ordered_by_integer strings ints =
    ((\(Symbol.Symbol _ i) -> i) <$> (sort (uncurry Symbol.Symbol <$> zip strings ints)))
      == sort (take (length strings) ints)

prop_increments :: [String] -> Bool
prop_increments keys =
    let unique = nub $ keys
    in (fmap ((flip Symbol.get) (foldl' (\s t -> snd $ Symbol.put t s) (Symbol.newTable 0) unique))
             unique
       )
              ==
        ((Just . uncurry Symbol.Symbol) <$> zip unique (iterate (+1) 0))

prop_no_double_increment :: String -> Int -> Bool
prop_no_double_increment s n =
    (Symbol.get s $ (foldl' (\t _ -> snd $ Symbol.put s t) (Symbol.newTable 0) (replicate ((abs n) + 1) n)))
      == (Just $ Symbol.Symbol s 0)

prop_return_symbol :: String -> Bool
prop_return_symbol s =
    let (sym, tbl) = Symbol.put s (Symbol.newTable 0)
    in Just sym == Symbol.get s tbl

spec :: Spec
spec = do
    describe "symbol equality" $ do
      it "is equal when the int portion is equal" $ property prop_equality_by_int

    describe "symbol order" $ do
      it "orders by the integer in the symbol" $ property prop_ordered_by_integer

    describe "storing strings in symbol table" $ do
      it "stores the strings with an integer" $ property prop_stores_with_integer
      it "increments the integer with each addition" $ property prop_increments
      it "does not increment the value of a symbol if it exists in the table" $ property prop_no_double_increment
      it "returns the symbol with put" $ property prop_return_symbol
