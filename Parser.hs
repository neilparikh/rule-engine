module Parser where
import Text.Parsec hiding (runParser)
import Control.Monad.Identity

type Parser a = ParsecT String () Identity a

data Cond = And Cond Cond -- Not implemented
          | Or  Cond Cond -- Not implemented
          | Cmp Pred Expr Expr
          deriving Show

data Pred = Eq
          | NotEq
          deriving Show

data Expr = Var String
          | Val Int
          deriving Show

type Action = String

data Rule = Rule Cond Action
          deriving Show

-- util functions

p :: Parser a -> String -> a
p = curry $ resolveError . uncurry runParser

runParser :: Parser a -> String -> Either ParseError a
runParser parser = parse parser "(source)"

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
    cond <- condParser
    return $ Rule cond action

actionParser :: Parser String
actionParser = many1 letter

condParser :: Parser Cond
condParser = do
    e1 <- exprParser
    spaces
    pred <- predParser
    spaces
    e2 <- exprParser
    return $ Cmp pred e1 e2

constString :: String -> a -> Parser a
constString s f = string s >> return f

predParser :: Parser Pred
predParser =     constString "==" Eq
             <|> constString "!=" NotEq

exprParser :: Parser Expr
exprParser =     (many1 digit >>= return . Val . read)
             <|> (many1 letter >>= return . Var)
