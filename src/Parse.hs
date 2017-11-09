module Parse where

import TigerTypes
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char

parse :: String -> Either String Expression
parse s = case Parsec.parse expressionParser "" s of
          Right exp -> Right exp
          Left e -> Left $ show e

expressionParser :: Parsec String () Expression
expressionParser = idParser

idParser :: Parsec String () Expression
idParser = fmap (LValExp . Id) $
  fmap concat (sequence [count 1 letter, many (alphaNum <|> (char '_'))])
