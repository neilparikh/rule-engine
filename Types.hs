{-# Language GADTs #-}
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

data Rule = Rule Condition  Action deriving Show

type Action = String

data Condition where
    Compound :: Conjunction -> Condition -> Condition -> Condition
    Compare  :: Predicate -> Expr -> Expr -> Condition
    deriving Show

data Predicate = Eq
               | NotEq
               deriving Show

data Conjunction = And
                 | Or
              deriving Show

data Expr = Var String
          | Val Int
          deriving Show
