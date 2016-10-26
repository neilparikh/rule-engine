module Parser where
import Text.Parsec

import Types
import ParseUtils
-- util functions

p :: Parser a -> String -> a
p = curry $ resolveError . uncurry (\parser -> runParser parser () "")

resolveError :: Either ParseError a -> a
resolveError = either (error . show) id

-- parsers

-- parses <action> if <rule>
ruleParser :: Parser Rule
ruleParser = do
    action <- actionParser
    spaces
    string "if"
    spaces
    condition <- conditionParser
    return $ Rule condition action

actionParser :: Parser String
actionParser = many1 letter

conditionParser :: Parser Condition
conditionParser =     try compoundParser
                  <|> compareParser

compoundParser :: Parser Condition
compoundParser = do
    e1 <- parens conditionParser
    spaces
    conjunction <- conjunctionParser
    spaces
    e2 <- parens conditionParser
    return $ Compound conjunction e1 e2

compareParser :: Parser Condition
compareParser = do
    e1 <- exprParser
    spaces
    predicate <- predicateParser
    spaces
    e2 <- exprParser
    return $ Compare predicate e1 e2

predicateParser :: Parser Predicate
predicateParser =     constString "==" Eq
                  <|> constString "!=" NotEq

exprParser :: Parser Expr
exprParser =     fmap (Val . read) (many1 digit)
             <|> fmap Var (many1 letter)

conjunctionParser :: Parser Conjunction
conjunctionParser =     constString "and" And
                    <|> constString "or" Or
