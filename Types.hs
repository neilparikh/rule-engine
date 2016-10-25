module Types where
import Text.Parsec (ParsecT)
import Control.Monad.Identity

type Parser a = ParsecT String () Identity a

data Cond = Mult Compound Cond Cond
          | Cmp Pred Expr Expr
          deriving Show

data Pred = Eq
          | NotEq
          deriving Show

data Compound = And
              | Or
              deriving Show

data Expr = Var String
          | Val Int
          deriving Show

type Action = String

data Rule = Rule Cond Action
          deriving Show

