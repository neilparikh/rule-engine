{-# Language OverloadedStrings #-}

import Data.Either (isRight)

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Parser (parseRule)
import Types (Rule)
import Arbitrary

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "parseRule" propParseShowRuleEqRule
       ] mempty

propParseShowRuleEqRule :: Rule -> Property
propParseShowRuleEqRule rule = property $ (parseRule . show) rule == Right rule
