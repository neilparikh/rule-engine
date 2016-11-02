-- {-# Language GADTs, DataKinds, KindSignatures, ExistentialQuantification, StandaloneDeriving #-}
module Types where

import Text.Parsec (Parsec)

type Parser a = Parsec String () a

-- data Rule = forall a. Rule (Condition a) Action
-- deriving instance Show Rule
-- data ConditionType = Comp | Cmp deriving Show
-- data Condition (a :: ConditionType) where
--     Compound :: Conjunction -> Condition a -> Condition b -> Condition 'Comp
--     Compare  :: Predicate -> Expr -> Expr -> Condition 'Cmp
-- deriving instance Show (Condition (a :: ConditionType))

data Rule = Rule Condition Action deriving (Show, Eq)

type Action = String

data Condition = Compound Conjunction Condition Condition
               | Compare Predicate Expr Expr
               deriving (Show, Eq)

data Predicate = Eq
               | NotEq
               deriving (Show, Eq)

data Conjunction = And
                 | Or
              deriving (Show, Eq)

data Expr = Var String
          | Val Int
          deriving (Show, Eq)
