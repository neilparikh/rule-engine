module Main where
import Parser (ruleParser)
import ParseUtils (applyParser)

main :: IO ()
main = do
    input <- getLine
    print $ applyParser ruleParser input
