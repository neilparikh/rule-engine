module Validate where
import Types

validateRule :: [Action] -> [String] -> Rule -> Bool
validateRule actions vars (Rule c a) = actionValid && validateCondition vars c
    where
    actionValid = a `elem` actions

validateCondition :: [String] -> Condition -> Bool
validateCondition validVars (Compare _ e1 e2) = isExprValid e1 && isExprValid e2
    where
    isExprValid (Val _) = True
    isExprValid (Var c) = c `elem` validVars
validateCondition validVars (Compound _ cond1 cond2) = validateCondition validVars cond1 && validateCondition validVars cond2
