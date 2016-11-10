{-# Language OverloadedStrings #-}

import Data.Either (isRight)

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Parser (parseRule)
import Types

-- Arbitrary defs

instance Arbitrary Rule where
    arbitrary = Rule <$> arbitrary <*> arbitrary

instance Arbitrary Action where
    arbitrary = Action <$> (listOf1 . choose $ ('A', 'Z'))

instance Arbitrary Condition where
    arbitrary = oneof [
        Compound <$> arbitrary <*> arbitrary <*> arbitrary,
        Compare <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Expr where
    arbitrary = oneof [
        Val <$> arbitrary,
        Var <$> (listOf1 . choose $ ('A', 'Z'))
        ]

instance Arbitrary Predicate where
    arbitrary = elements [Eq, NotEq]

instance Arbitrary Conjunction where
    arbitrary = elements [And, Or]

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "parseRule" propParseShowRuleEqRule
       ] mempty

propParseShowRuleEqRule :: Rule -> Property
propParseShowRuleEqRule rule = property $ (parseRule . show) rule == Right rule
