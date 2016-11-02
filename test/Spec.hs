import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Parser (applyParser, exprParser, parseRule)
import Types

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "canParseInt" propCanParseInt
       , testCase "parseRule" testParseRule
       ] mempty

propCanParseInt :: Int -> Property
propCanParseInt x = True ==> applyParser exprParser (show x) == (Right . Val) x

exampleRuleStr :: String
exampleRuleStr = "Show if ((1 == name) or (count != -3)) and (age == 2)"

exampleRule :: Rule
exampleRule = Rule (Compound And (Compound Or (Compare Eq (Val 1) (Var "name"))
                                              (Compare NotEq (Var "count") (Val (-3))))
                                 (Compare Eq (Var "age") (Val 2))) "Show"

testParseRule :: Assertion
testParseRule = assertEqual "" (parseRule exampleRuleStr) (Right exampleRule)
