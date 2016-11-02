import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Parser (applyParser, exprParser)
import Types

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "canParseInt" propCanParseInt
       ] mempty

propCanParseInt :: Int -> Property
propCanParseInt x = True ==> applyParser exprParser (show x) == (Right . Val) x
