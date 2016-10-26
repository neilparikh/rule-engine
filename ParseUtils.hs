module ParseUtils where
import Text.Parsec (string, char, spaces)
import Types

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
