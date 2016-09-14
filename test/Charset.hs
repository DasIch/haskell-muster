module Charset where

import Prelude hiding (any, elem, notElem, intersect)

import qualified Data.List as L

import Test.Tasty
import Test.Tasty.QuickCheck

import Muster.Internal.Charset


charsetTests :: TestTree
charsetTests = testGroup "Muster.Internal.Charset"
  [ testProperty "none" propNone
  , testProperty "any" propAny
  , testProperty "elem" propElem
  , testProperty "insert" propInsert
  , testProperty "remove" propRemove
  , intersectTests
  , testProperty "oneOf" propOneOf
  ]


propNone :: Char -> Bool
propNone c = c `notElem` none


propAny :: Char -> Bool
propAny c = c `elem` any


propElem :: Char -> Charset -> Bool
propElem c cs = c `elem` cs /= c `notElem` cs


propInsert :: Char -> Charset -> Property
propInsert c cs =
  c `notElem` cs ==>
  c `elem` (c `insert` cs)


propRemove :: Char -> Charset -> Property
propRemove c cs =
  c `elem` cs ==>
  c `notElem` (c `remove` cs)


intersectTests :: TestTree
intersectTests = testGroup "intersect"
  [ testProperty "produces intersection" propIntersect
  , testProperty "maintains order invariant" propIntersectOrderInvariant
  ]


propIntersect :: Char -> Charset -> Charset -> Property
propIntersect c ca cb =
  (c `elem` ca && c `elem` cb) === c `elem` intersect ca cb


propIntersectOrderInvariant :: Charset -> Charset -> Bool
propIntersectOrderInvariant ca cb =
  case ca `intersect` cb of
    AnyOf xs -> xs == L.sort xs
    NoneOf xs -> xs == L.sort xs


propOneOf :: Charset -> Bool
propOneOf cs =
  case oneOf cs of
    Just c -> c `elem` cs
    Nothing -> True
