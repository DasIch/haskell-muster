{-# LANGUAGE OverloadedStrings #-}
module Regex where

import Test.Tasty
import Test.Tasty.HUnit

import Muster


regexTests :: TestTree
regexTests = testGroup "Muster.Internal.Regex"
  [ showTests
  , fromStringTests
  ]


showTests :: TestTree
showTests = testGroup "show"
  [ showConcatenationTests
  , testCase "KleeneStar" testShowKleeneStar
  , testCase "Or" testShowOr
  , testCase "And" testShowAnd
  , testCase "Not" testShowNot
  ]


showConcatenationTests :: TestTree
showConcatenationTests = testGroup "Concatenation"
  [ testCase "with Symbol on left" testShowConcatenationLeftSymbol
  , testCase "with Symbol on right" testShowConcatenationRightSymbol
  , testCase "with two Symbols" testShowConcatenationTwoSymbols
  , testCase "with left leaning string" testShowConcatenationStringLeftLeaning
  , testCase "with right leaning string" testShowConcatenationStringRightLeaning
  ]


testShowConcatenationLeftSymbol :: Assertion
testShowConcatenationLeftSymbol =
    show (Concatenation "a" None) @?= "Concatenation \"a\" None"


testShowConcatenationRightSymbol :: Assertion
testShowConcatenationRightSymbol =
    show (Concatenation None "a") @?= "Concatenation None \"a\""


testShowConcatenationTwoSymbols :: Assertion
testShowConcatenationTwoSymbols =
  show (Concatenation "a" "b") @?= "\"ab\""


testShowConcatenationStringLeftLeaning :: Assertion
testShowConcatenationStringLeftLeaning =
  show (Concatenation (Concatenation "a" "b") "c") @?= "\"abc\""


testShowConcatenationStringRightLeaning :: Assertion
testShowConcatenationStringRightLeaning =
  show (Concatenation "a" (Concatenation "b" "c")) @?= "\"abc\""


testShowKleeneStar :: Assertion
testShowKleeneStar = show (KleeneStar "a") @?= "KleeneStar \"a\""


testShowOr :: Assertion
testShowOr = show (Or "a" "b") @?= "Or \"a\" \"b\""


testShowAnd :: Assertion
testShowAnd = show (And "a" "b") @?= "And \"a\" \"b\""


testShowNot :: Assertion
testShowNot = show (Not "a") @?= "Not \"a\""


fromStringTests :: TestTree
fromStringTests = testGroup "fromString"
 [ testCase "empty string" testEmptyString
 , testCase "one char string" testOneCharString
 , testCase "two char string" testTwoCharString
 , testCase "three char string" testThreeCharString
 ]


testEmptyString :: Assertion
testEmptyString = fromString "" @?= Epsilon


testOneCharString :: Assertion
testOneCharString = fromString "a" @?= Symbol 'a'


testTwoCharString :: Assertion
testTwoCharString = fromString "ab" @?= Concatenation (Symbol 'a') (Symbol 'b')


testThreeCharString :: Assertion
testThreeCharString = fromString "abc" @?=
  Concatenation (Concatenation (Symbol 'a') (Symbol 'b'))
                (Symbol 'c')
