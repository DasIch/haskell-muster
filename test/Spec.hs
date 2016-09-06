{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Text.Arbitrary
import qualified Data.Text as T
import Data.List

import Muster


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "tests" [showTests, matchTests]


showTests :: TestTree
showTests = testGroup "show"
  [ testCase "Concatenation" testShowConcatenation
  , testCase "Concatenation (1-Symbol)" testShowConcatenationOneSymbol
  , testCase "Concatenation (2-Symbol)" testShowConcatenationTwoSymbol
  , testCase "Concatenation (3-Symbol)" testShowConcatenationThreeSymbol
  , testCase "KleeneStar" testShowKleeneStar
  , testCase "Or" testShowOr
  , testCase "And" testShowAnd
  , testCase "Not" testShowNot
  ]


testShowConcatenation :: Assertion
testShowConcatenation =
  show ("a" <|> "b" <.> "c" <|> "d") @?= "Concatenation (Or (\"a\") (\"b\")) (Or (\"c\") (\"d\"))"


testShowConcatenationOneSymbol :: Assertion
testShowConcatenationOneSymbol =
  show ("a" <.> "b" <|> "c") @?= "Concatenation \"a\" (Or (\"b\") (\"c\"))"


testShowConcatenationTwoSymbol :: Assertion
testShowConcatenationTwoSymbol = show ("ab" :: Regex) @?= "\"ab\""


testShowConcatenationThreeSymbol :: Assertion
testShowConcatenationThreeSymbol = show ("abc" :: Regex) @?= "\"abc\""


testShowKleeneStar :: Assertion
testShowKleeneStar = show (KleeneStar "a") @?= "KleeneStar (\"a\")"


testShowOr :: Assertion
testShowOr = show ("a" <|> "b") @?= "Or (\"a\") (\"b\")"


testShowAnd :: Assertion
testShowAnd = show ("a" <&> "b") @?= "And (\"a\") (\"b\")"


testShowNot :: Assertion
testShowNot = show (Not "a") @?= "Not \"a\""


matchTests :: TestTree
matchTests = testGroup "match"
  [ testProperty "Symbol" propMatchSymbol
  , testProperty "Symbol wrong char" propNotMatchSymbol
  , testProperty "Concatenation (String)" propMatchText
  , testProperty "Concatenation (String) wrong string" propNotMatchText
  , testProperty "many" propMatchMany
  , testProperty "many1" propMatchMany1
  , testProperty "Or" propMatchOr
  , testProperty "And" propMatchAnd
  , testProperty "Not" propMatchNot
  ]


propMatchSymbol :: Char -> Bool
propMatchSymbol c = match (Symbol c) (T.singleton c)


propNotMatchSymbol :: Char -> Char -> Property
propNotMatchSymbol a b = a /= b ==> not (match (Symbol a) (T.singleton b))


propMatchText :: Text -> Bool
propMatchText text = match (fromText text) text


propNotMatchText :: Text -> Text -> Property
propNotMatchText a b = a /= b ==> not (match (fromText a) b)


propMatchMany :: Property
propMatchMany = forAll genRepeatedText (uncurry propMatchRepeatedText')
  where propMatchRepeatedText' text = match (many (fromText text))
        genRepeatedText = do
          text <- arbitrary
          n <- choose (0, 10) -- chosen for performance
          return (text, T.replicate n text)


propMatchMany1 :: Property
propMatchMany1 = forAll genRepeatedText (uncurry propMatchRepeatedText')
  where propMatchRepeatedText' text = match (many1 (fromText text))
        genRepeatedText = do
          text <- arbitrary
          n <- choose (1, 10) -- chosen for performance
          return (text, T.replicate n text)


propMatchOr :: Text -> Text -> Property
propMatchOr a b = match regex a .&&. match regex b
  where regex = fromText a <|> fromText b


propMatchAnd :: Char -> Char -> Bool
propMatchAnd a b =
  if a == b
    then match regex a' && match regex b'
    else not (match regex a' || match regex b')
  where a' = T.singleton a
        b' = T.singleton b
        regex = Symbol a <&> Symbol b


propMatchNot :: Text -> Text -> Bool
propMatchNot a b =
  if match (fromText a) b
    then not $ match (Not (fromText a)) b
    else match (Not (fromText a)) b
