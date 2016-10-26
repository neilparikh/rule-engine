module ParseUtils where
import Text.Parsec hiding (runParser)
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
