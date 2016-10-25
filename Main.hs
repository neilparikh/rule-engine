module Main where
import Parser (p, ruleParser)

main = print $ p ruleParser "block if (1 == 2) or (2 != 3)"
