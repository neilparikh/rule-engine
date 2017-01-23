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
ruleParser = do
    action <- actionParser
    wrapWithSpaces $ string "if"
    condition <- conditionParser
    return $ Rule condition action

actionParser :: Parser Action
actionParser = Action <$> many1 letter

conditionParser :: Parser Condition
conditionParser =     try compoundParser
                  <|> compareParser

-- parses <cond> (or|and) <cond>
compoundParser :: Parser Condition
compoundParser = do
    e1 <- parens conditionParser
    conjunction <- wrapWithSpaces conjunctionParser
    e2 <- parens conditionParser
    return $ Compound conjunction e1 e2

-- parses <expr> (==|!=) <expr>
compareParser :: Parser Condition
compareParser = do
    e1 <- exprParser
    predicate <- wrapWithSpaces predicateParser
    e2 <- exprParser
    return $ Compare predicate e1 e2

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
