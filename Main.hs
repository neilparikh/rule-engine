module Main where
import Parser (parseRule)
import Validate

main :: IO ()
main = do
    input <- getLine
    case (parseRule input) of
        Right rule -> do
            print rule
            print $ validateRule ["Show", "Hide"] ["age", "name", "count"] rule
        Left err -> error . show $ err
