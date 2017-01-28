module Parser where
import Text.Parsec

import Types
import ParseUtils

parseRule :: String -> Either ParseError Rule
parseRule = applyParser ruleParser

applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

-- parses <action> if <rule>
ruleParser :: Parser Rule
ruleParser = flip Rule <$> actionParser <* wrapWithSpaces (string "if") <*> conditionParser

actionParser :: Parser Action
actionParser = Action <$> many1 letter

conditionParser :: Parser Condition
conditionParser =     try compoundParser
                  <|> compareParser

-- parses <cond> (or|and) <cond>
compoundParser :: Parser Condition
compoundParser = compoundFlipped <$> condWithParens <*> wrapWithSpaces conjunctionParser <*> condWithParens
    where
    compoundFlipped a b c = Compound b a c
    condWithParens = parens conditionParser

-- parses <expr> (==|!=) <expr>
compareParser :: Parser Condition
compareParser = compareFlipped <$> exprParser <*> wrapWithSpaces predicateParser <*> exprParser
    where
    compareFlipped a b c = Compare b a c

predicateParser :: Parser Predicate
predicateParser =     constString "==" Eq
                  <|> constString "!=" NotEq

exprParser :: Parser Expr
exprParser =     Val <$> intParser
             <|> Var <$> (many1 letter)

intParser :: Parser Int
intParser = do
    negSign <- option ' ' (char '-')
    num <- many1 digit
    return . read $ (negSign:num)

conjunctionParser :: Parser Conjunction
conjunctionParser =     constString "and" And
                    <|> constString "or" Or
