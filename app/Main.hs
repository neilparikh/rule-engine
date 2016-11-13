{-# Language OverloadedStrings #-}
module Main where
import Data.Map as M

import Parser (parseRule)
import Validate
import Types
import Eval

vars :: M.Map String Int
vars = M.fromList [("age", 2), ("foo", 1), ("count", 10)]

showUser :: Int -> IO ()
showUser i = putStrLn $ "showing " ++ show i

hideUser :: Int -> IO ()
hideUser i = putStrLn $ "hiding " ++ show i

actions :: M.Map Action (Int -> IO ())
actions = M.fromList [("Show", showUser), ("Hide", hideUser)]

main :: IO ()
main = do
    input <- getLine
    case parseRule input of
        Right rule -> do
            print rule
            case (validateRule ["Show", "Hide"] ["age", "foo", "count"] rule) of
                True -> runRule rule vars actions 1
                False -> putStrLn "invalid rule"

        Left err -> error . show $ err
