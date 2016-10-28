module ParseUtils where
import Text.Parsec (string, char, spaces, ParseError, runParser)
import Types

applyParser :: Parser a -> String -> a
applyParser = curry $ resolveError . uncurry (\parser -> runParser parser () "")

resolveError :: Either ParseError a -> a
resolveError = either (error . show) id

constString :: String -> a -> Parser a
constString s f = string s >> return f

parens :: Parser a -> Parser a
parens subParser = do
    char '('
    spaces
    a <- subParser
    spaces
    char ')'
    return a
