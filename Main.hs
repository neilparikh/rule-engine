module Main where
import Parser (ruleParser)
import ParseUtils (applyParser)
import Validate

main :: IO ()
main = do
    input <- getLine
    let rule = applyParser ruleParser input
    print $ validateRule "Show" ["age", "name", "count"] rule
