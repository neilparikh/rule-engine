module Arbitrary where
import Test.QuickCheck

import Types
-- Arbitrary defs

instance Arbitrary Rule where
    arbitrary = Rule <$> arbitrary <*> arbitrary

instance Arbitrary Action where
    arbitrary = Action <$> (listOf1 . choose $ ('A', 'Z'))

instance Arbitrary Condition where
    arbitrary = sized arbitrary'
        where
        arbitrary' 0 = Compare <$> arbitrary <*> arbitrary <*> arbitrary
        arbitrary' n = oneof [
            Compound <$> arbitrary <*> arbitrary' (n-1) <*> arbitrary' (n-1),
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

