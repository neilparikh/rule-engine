module Main where
import Parser (ruleParser)
import ParseUtils (applyParser)
import Validate

main :: IO ()
main = do
    input <- getLine
    let rule = applyParser ruleParser input
    print rule
    print $ validateRule ["Show", "Hide"] ["age", "name", "count"] rule
