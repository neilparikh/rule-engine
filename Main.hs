module Main where
import Parser (p, ruleParser)

main = do
    input <- getLine
    print $ p ruleParser input
