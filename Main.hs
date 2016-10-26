module Main where
import Parser (p, ruleParser)

main :: IO ()
main = do
    input <- getLine
    print $ p ruleParser input
