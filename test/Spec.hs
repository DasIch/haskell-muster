{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (not)
import qualified Data.Bool as B

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Text.Arbitrary
import qualified Data.Text as T

import Muster

import Regex
import Charset


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "\nMuster" [regexTests, charsetTests, matchTests]


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
propNotMatchSymbol a b = a /= b ==> B.not (match (Symbol a) (T.singleton b))


propMatchText :: Text -> Bool
propMatchText text = match (fromText text) text


propNotMatchText :: Text -> Text -> Property
propNotMatchText a b = a /= b ==> B.not (match (fromText a) b)


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
    else B.not (match regex a' || match regex b')
  where a' = T.singleton a
        b' = T.singleton b
        regex = Symbol a <&> Symbol b


propMatchNot :: Text -> Text -> Bool
propMatchNot a b =
  if match (fromText a) b
    then B.not $ match (not (fromText a)) b
    else match (not (fromText a)) b
