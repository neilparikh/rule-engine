-- {-# Language GADTs, DataKinds, KindSignatures, ExistentialQuantification, StandaloneDeriving #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Types where

import Text.Parsec (Parsec)
import Data.String (IsString)

type Parser a = Parsec String () a

-- data Rule = forall a. Rule (Condition a) Action
-- deriving instance Show Rule
-- data ConditionType = Comp | Cmp deriving Show
-- data Condition (a :: ConditionType) where
--     Compound :: Conjunction -> Condition a -> Condition b -> Condition 'Comp
--     Compare  :: Predicate -> Expr -> Expr -> Condition 'Cmp
-- deriving instance Show (Condition (a :: ConditionType))

data Rule = Rule Condition Action deriving Eq

instance Show Rule where
    show (Rule condition action) = show action ++ " if " ++ show condition

newtype Action = Action String deriving (IsString, Eq)

instance Show Action where
    show (Action str) = str

data Condition = Compound Conjunction Condition Condition
               | Compare Predicate Expr Expr
               deriving Eq

instance Show Condition where
    show (Compound conjunction c1 c2) =
        "(" ++ show c1 ++ ") " ++ show conjunction ++ " (" ++ show c2 ++ ")"
    show (Compare predicate e1 e2)    =
        show e1 ++ " " ++ show predicate ++ " " ++ show e2

data Predicate = Eq
               | NotEq
               deriving Eq

instance Show Predicate where
    show Eq = "=="
    show NotEq = "!="

data Conjunction = And
                 | Or
                 deriving Eq

instance Show Conjunction where
    show And = "and"
    show Or  = "or"

data Expr = Var String
          | Val Int
          deriving Eq

instance Show Expr where
    show (Var s) = s
    show (Val i) = show i
