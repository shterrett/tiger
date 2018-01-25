module Typing where

import TigerTypes (TypeFields, TypeName)

data ProgramType =
    TigerInt Integer
    | TigerStr String
    | Record TypeFields
    | Array TypeName
    | Nil
    | Unit
    | Name TypeName

instance Eq ProgramType where
    (==) (TigerInt i) (TigerInt j) = i == j
    (==) (TigerStr s) (TigerStr t) = s == t
    (==) _ _ = False


