module Parse where

import TigerTypes
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char
import Data.Char (isSpace)

parse :: String -> Either String Expression
parse s = case Parsec.parse expressionParser "" (prepareProgram s) of
          Right exp -> Right exp
          Left e -> Left $ show e
    where prepareProgram = stripLeadingWhitespace . stripComments

stripComments :: String -> String
stripComments = commentStripper Take ""

data CommentState = Take | Pass
commentStripper :: CommentState -> String -> String -> String
commentStripper _ taken [] = reverse taken
commentStripper Take taken ('/':'*':rest) = commentStripper Pass taken rest
commentStripper Take taken (c:rest) = commentStripper Take (c:taken) rest
commentStripper Pass taken ('*':'/':rest) = commentStripper Take taken rest
commentStripper Pass taken (c:rest) = commentStripper Pass taken rest

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = dropWhile isSpace

expressionParser :: Parsec String () Expression
expressionParser =
    try ((\(ifExp, thenExp, elseExp) -> IfThenElse ifExp thenExp elseExp) <$> ifThenElseParser)
    <|> try (uncurry IfThen <$> ifThenParser)
    <|> try ((\(t, e1, e2) -> ArrayCreation t e1 e2) <$> arrayCreateParser)
    <|> try (DecExp <$> declarationParser)
    <|> try (Sequence <$> (between lparen rparen $ sequenceParser))
    <|> try (Grouped <$> groupParser)
    <|> try ((uncurry Let) <$> letParser)
    <|> try (Negation <$> negationParser)
    <|> try (uncurry FunctionCall <$> funcParser)
    <|> try binopParser
    <|> try (uncurry RecordCreation <$> recordCreateParser)
    <|> try (uncurry Assignment <$> assignmentParser)
    <|> try (uncurry While <$> whileParser)
    <|> try ((\(var, start, end, exp) -> For var start end exp) <$> forParser)
    <|> try (LValExp <$> lvalueParser)
    <|> try (StringLiteral <$> stringParser)
    <|> try (IntLiteral <$> intParser)
    <|> try (const Nil <$> nilParser)
    <|> try (const NoValue <$> noValueParser)

binopableParser :: Parsec String () Expression
binopableParser =
    try (uncurry FunctionCall <$> funcParser)
    <|> try (LValExp <$> lvalueParser)
    <|> try (Sequence <$> (between lparen rparen $ sequenceParser))
    <|> try (IntLiteral <$> intParser)
    <|> try (StringLiteral <$> stringParser)
    <|> try (Negation <$> negationParser)
    <|> try ((\(ifExp, thenExp, elseExp) -> IfThenElse ifExp thenExp elseExp) <$> ifThenElseParser)
    <|> try (uncurry IfThen <$> ifThenParser)
    <|> try (Grouped <$> groupParser)

declarationParser :: Parsec String () Declaration
declarationParser = try typeDeclarationParser
  <|> try variableDeclarationParser
  <|> try functionDeclarationParser
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
      e <- expInBrackets
      return (\l -> ArraySubscript l e)
    expInBrackets =
      (try (lsquare >> spaces >> rsquare) >> fail "square brackets cannot be empty")
      <|> between lsquare rsquare expressionParser

nilParser :: Parsec String () ()
nilParser = const () <$> (string "nil" >> notFollowedBy (alphaNum <|> (char '_')))

noValueParser :: Parsec String () ()
noValueParser = const () <$> between lparen rparen spaces

sequenceParser :: Parsec String () [Expression]
sequenceParser =  sepBy expressionParser semicolon

intParser :: Parsec String () Integer
intParser = read <$> ((many1 digit) <* notFollowedBy (alphaNum <|> (char '_')))

stringParser :: Parsec String () String
stringParser =
    let escapedQuote = try $ fmap (const '"') (string "\\\"")
        notQuote = noneOf ['"']
        withinQuotes = between (char '"') (char '"')
    in withinQuotes (many $ escapedQuote <|> notQuote)

negationParser :: Parsec String () Expression
negationParser = (try (string "- " <* spaces) >> fail "Cannot have space after negation")
                 <|> (char '-' >> expressionParser)

funcParser :: Parsec String () (Atom, [Expression])
funcParser =
    let argList = const [] <$> try (lparen >> space >> spaces >> rparen)
                  <|> const [] <$> try (lparen >> rparen)
                  <|> try (between lparen rparen (sepBy expressionParser comma))
    in (,) <$> atomParser <*> argList

binopParser :: Parsec String () Expression
binopParser = (lookAhead $ try (manyTill (alphaNum <|> space <|> char '_') (try operatorParser))) >> chainl1 binopableParser operator
  where operator = BinOp <$> operatorParser

recordCreateParser :: Parsec String () (Atom, [(Atom, Expression)])
recordCreateParser = (,) <$> (atomParser <* spaces) <*> (between lbrace rbrace (sepBy fieldPairParser comma))
  where fieldPairParser = pairAtoms <$> atomParser <*> equals <*> expressionParser
        pairAtoms field _ expression = (field, expression)

arrayCreateParser :: Parsec String () (Atom, Expression, Expression)
arrayCreateParser = (,,) <$>
                    (atomParser <* spaces) <*>
                    (between lsquare rsquare expressionParser <* spaces) <*>
                    (string "of" >> spaces >> expressionParser)

ifThenElseParser :: Parsec String () (Expression, Expression, Expression)
ifThenElseParser = (,,) <$>
                   (string "if" >> spaces >> expressionParser <* spaces) <*>
                   (string "then" >> spaces >> expressionParser <* spaces) <*>
                   (string "else" >> spaces >> expressionParser <* spaces)

ifThenParser :: Parsec String () (Expression, Expression)
ifThenParser = (,) <$>
               (string "if" >> spaces >> expressionParser <* spaces) <*>
               (string "then" >> spaces >> expressionParser <* spaces)

assignmentParser :: Parsec String () (LValue, Expression)
assignmentParser = (,) <$> (lvalueParser <* (spaces >> string ":=" >> spaces)) <*> expressionParser

whileParser :: Parsec String () (Expression, Expression)
whileParser = (,) <$>
              (string "while" >> spaces >> expressionParser) <*>
              (spaces >> string "do" >> spaces >> expressionParser)

forParser :: Parsec String () (Atom, Expression, Expression, Expression)
forParser = (,,,) <$>
            (string "for" >> spaces >> atomParser) <*>
            (spaces >> string ":=" >> spaces >> expressionParser) <*>
            (spaces >> string "to" >> spaces >> expressionParser) <*>
            (spaces >> string "do" >> spaces >> expressionParser)

breakParser :: Parsec String () ()
breakParser = (spaces >> string "break" >> spaces) >> notFollowedBy (alphaNum <|> char '_')

letParser :: Parsec String () ([Declaration], [Expression])
letParser = (,) <$>
            between (string "let" >> spaces)
                    (string "in" >> spaces)
                    (sepEndBy1 declarationParser spaces) <*>
            (spaces >> sequenceParser <* spaces <* string "end")

groupParser :: Parsec String () Expression
groupParser = between lparen rparen expressionParser

leftRec :: Parsec String () a -> Parsec String () (a -> a) -> Parsec String () a
leftRec p op = rest =<< p
  where
    rest x = do f <- op
                rest (f x)
          <|> return x

typeParser :: Parsec String () Type
typeParser = try (fmap RecordOf $ typeFieldParser lbrace rbrace)
             <|> try (fmap ArrayOf (string "array of" >> spaces >> typeNameParser))
             <|> fmap  TypeId typeNameParser

typeFieldParser :: Parsec String () String -> Parsec String () String -> Parsec String () TypeFields
typeFieldParser lsep rsep = between lsep rsep (sepBy fieldPairParser comma)
  where fieldPairParser = pairAtoms <$> sequence [atomParser, colon, typeNameParser]
        pairAtoms (field:_:typeId:[]) = (field, typeId)

atomParser :: Parsec String () Atom
atomParser = (try reservedWordsParser >>= (\w -> unexpected $ "Reserved Word " ++ w))
    <|> fmap concat (sequence [count 1 letter, many (alphaNum <|> (char '_'))])

typeNameParser :: Parsec String () TypeName
typeNameParser = atomParser

reservedWordsParser :: Parsec String () String
reservedWordsParser =
    let reservedWords = [ "let"
                        , "in"
                        , "if"
                        , "then"
                        , "else"
                        , "end"
                        , "var"
                        , "type"
                        , "function"
                        , "array"
                        , "of"
                        , "nil"
                        , "while"
                        , "for"
                        , "to"
                        , "break"
                        ]
    in choice $ (\w -> try (string w) <* notFollowedBy (alphaNum <|> char '_')) <$> reservedWords

colon = try $ charToString (char ':' <* spaces)
semicolon = try $ charToString (spaces >> char ';' <* spaces)
equals = try $ charToString (spaces >> char '=' <* spaces)
comma = try $ charToString (spaces >> char ',' <* spaces)
lbrace = try $ charToString (char '{' <* spaces)
rbrace = try $ charToString (spaces >> char '}')
lparen = try $ charToString (char '(' <* spaces)
rparen = try $ charToString (spaces >> char ')')
lsquare = try $ charToString (char '[' <* spaces)
rsquare = try $ charToString (spaces >> char ']')

charToString :: Parsec String () Char -> Parsec String () String
charToString = fmap (\c -> [c])

plus = const Addition <$> (spaces >> (char '+') <* spaces)
minus = const Subtraction <$> (spaces >> (char '-') <* spaces)
multby = const Multiplication <$> (spaces >> (char '*') <* spaces)
divby = const Division <$> (spaces >> (char '/') <* spaces)
greaterThan = const GreaterThan <$> (spaces >> ((char '>') <* (notFollowedBy $ char '=')) <* spaces)
lessThan = const LessThan <$> (spaces >> ((char '<') <*  (notFollowedBy $ char '=')) <* spaces)
greaterThanOrEqual = const GreaterThanOrEqual <$> (spaces >> (string ">=") <* spaces)
lessThanOrEqual = const LessThanOrEqual <$> (spaces >> (string "<=") <* spaces)
equality = const Equality <$> (spaces >> (char '=') <* spaces)
nonEquality = const NonEquality <$> (spaces >> (string "<>") <* spaces)
boolAnd = const And <$> (spaces >> (char '&') <* spaces)
boolOr = const Or <$> (spaces >> (char '|') <* spaces)

operatorParser :: Parsec String () Operator
operatorParser =
    try plus
    <|> try minus
    <|> try multby
    <|> try divby
    <|> try equality
    <|> try nonEquality
    <|> try greaterThan
    <|> try lessThan
    <|> try greaterThanOrEqual
    <|> try lessThanOrEqual
    <|> try boolAnd
    <|> try boolOr
