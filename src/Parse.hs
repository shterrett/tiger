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
idParser = fmap (LValExp . Id) $ atomParser

commentParser :: Parsec String () Expression
commentParser = fmap Comment $
  string "/*" >> manyTill anyChar (try $ string "*/")

typeFieldParser :: Parsec String () TypeFields
typeFieldParser = between lbrace rbrace (sepBy fieldPairParser comma)
  where fieldPairParser = pairAtoms <$> sequence [atomParser, colon, typeNameParser]
        pairAtoms (field:_:typeId:[]) = (field, typeId)

atomParser :: Parsec String () Atom
atomParser = fmap concat (sequence [count 1 letter, many (alphaNum <|> (char '_'))])

typeNameParser :: Parsec String () TypeName
typeNameParser = atomParser

colon = try (string ": ") <|> string ":"
comma = try (string ", ") <|> string ","
lbrace = try (string "{ ") <|> string "{"
rbrace = try (string " }") <|> string "}"
