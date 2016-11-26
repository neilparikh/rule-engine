module Arbitrary where
import Test.QuickCheck

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

