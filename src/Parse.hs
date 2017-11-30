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
expressionParser =
    try (Comment <$> commentParser)
    <|> try (DecExp <$> declarationParser)
    <|> try (LValExp <$> lvalueParser)
    <|> try (const Nil <$> nilParser)
    <|> try (Sequence <$> sequenceParser)

commentParser :: Parsec String () String
commentParser = string "/*" >> manyTill anyChar (try $ string "*/")

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

lvalueParser :: Parsec String () LValue
lvalueParser = leftRec idParser (recordAccessModifier <|> arraySubscriptModifier)
  where
    idParser = Id <$> atomParser
    recordAccessModifier = do
      a <- char '.' >> atomParser
      return (\l -> RecordAccess l a)
    arraySubscriptModifier = do
      e <- between lsquare rsquare expressionParser
      return (\l -> ArraySubscript l e)

nilParser :: Parsec String () ()
nilParser = const () <$> (string "nil" >> notFollowedBy (alphaNum <|> (char '_')))

sequenceParser :: Parsec String () [Expression]
sequenceParser = between lparen rparen $ sepBy expressionParser semicolon

leftRec :: Parsec String () a -> Parsec String () (a -> a) -> Parsec String () a
leftRec p op = rest =<< p
  where
    rest x = do f <- op
                rest (f x)
          <|> return x

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

colon = try $ charToString (char ':' <* spaces)
semicolon = try $ charToString (spaces >> char ';' <* spaces)
comma = try $ charToString (char ',' <* spaces)
lbrace = try $ charToString (char '{' <* spaces)
rbrace = try $ charToString (spaces >> char '}')
lparen = try $ charToString (char '(' <* spaces)
rparen = try $ charToString (spaces >> char ')')
lsquare = try $ charToString (char '[' <* spaces)
rsquare = try $ charToString (spaces >> char ']')

charToString :: Parsec String () Char -> Parsec String () String
charToString = fmap (\c -> [c])
