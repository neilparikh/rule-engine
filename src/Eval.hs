module Eval where
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)

import Types

runRule :: Rule -> M.Map String Int -> M.Map Action (a -> IO ()) -> a -> IO ()
runRule (Rule condition action) varsMap actionsMap context =
    case evalCond condition varsMap of
        Just True -> case M.lookup action actionsMap of
            Just f -> f context
            Nothing -> error "action not found"
        Just False -> return ()
        Nothing -> error "error evaluating the condition"

predicateToFunction :: (Eq a) => Predicate -> (a -> a -> Bool)
predicateToFunction    Eq = (==)
predicateToFunction NotEq = (/=)

conjunctionToFunction :: Conjunction -> (Bool -> Bool -> Bool)
conjunctionToFunction And = (&&)
conjunctionToFunction Or  = (||)

evalCond :: Condition -> M.Map String Int -> Maybe Bool
evalCond (Compound conj cond1 cond2) varsMap = liftA2 (conjunctionToFunction conj) a b
    where
    a = evalCond cond1 varsMap
    b = evalCond cond2 varsMap
evalCond (Compare pred e1 e2) varsMap = (predicateToFunction pred) <$> a <*> b
    where
    a = evalExpr e1 varsMap
    b = evalExpr e2 varsMap

evalExpr :: Expr -> M.Map String Int -> Maybe Int
evalExpr (Var s) varsMap = M.lookup s varsMap
evalExpr (Val i) varsMap = Just i
