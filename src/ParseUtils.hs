module ParseUtils where
import Text.Parsec (string, char, spaces)
import Types (Parser)

wrapWith :: Parser a -> Parser b -> Parser b
wrapWith wrapper subParser = wrapper *> subParser <* wrapper

wrapWithSpaces :: Parser a -> Parser a
wrapWithSpaces = wrapWith spaces

constString :: String -> a -> Parser a
constString s f = string s *> return f

parens :: Parser a -> Parser a
parens subParser = char '(' *> wrapWithSpaces subParser <* char ')'
