module Environment where

import qualified Data.Map.Strict as Map
import Symbol (Symbol)
import AST (Expression)

type Environment a = [Map.Map Symbol a]

fromList :: [(Symbol, a)] -> Environment a
fromList = pure . Map.fromList

lookup :: Symbol -> Environment a -> Maybe a
lookup s (e:_) = Map.lookup s e

pushScope :: [(Symbol, a)] -> Environment a -> Environment a
pushScope n es@(e:_) = (Map.union (Map.fromList n) e):es

popScope :: Environment a -> Environment a
popScope = tail

addBinding :: (Symbol, a) -> Environment a -> Environment a
addBinding (k, v) (e:es) = (Map.insert k v e):es
