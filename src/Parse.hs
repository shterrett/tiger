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

declarationParser :: Parsec String () Declaration
declarationParser = try typeDeclarationParser
  <|> try variableDeclarationParser
  <|> functionDeclarationParser
  where typeDeclarationParser = TypeDec
                                <$> (string "type" >> spaces >> typeNameParser)
                                <*> (spaces >> char '=' >> spaces >> typeParser)
        variableDeclarationParser = VarDec
                                    <$> (string "var" >> spaces >> atomParser)
                                    <*> (optionMaybe $ try (spaces >> char ':' >> spaces >> typeNameParser))
                                    <*> (spaces >> string ":=" >> spaces >> expressionParser)
        functionDeclarationParser = FnDec
                                    <$> (string "function" >> spaces >> atomParser)
                                    <*> (spaces >> typeFieldParser lparen rparen)
                                    <*> (optionMaybe $ try (spaces >> char ':' >> spaces >> typeNameParser))
                                    <*> (spaces >> char '=' >> spaces >> expressionParser)

typeParser :: Parsec String () Type
typeParser = try (fmap RecordOf $ typeFieldParser lbrace rbrace)
             <|> try (fmap ArrayOf (string "array of " >> typeNameParser))
             <|> fmap  TypeId typeNameParser

typeFieldParser :: Parsec String () String -> Parsec String () String -> Parsec String () TypeFields
typeFieldParser lsep rsep = between lsep rsep (sepBy fieldPairParser comma)
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
lparen = try (string "( ") <|> string "("
rparen = try (string ") ") <|> string ")"
