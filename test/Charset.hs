module Charset where

import Prelude hiding (any, elem, notElem)

import qualified Data.List as L

import Test.Tasty
import Test.Tasty.QuickCheck

import Muster.Internal.Charset


success :: Property
success = True === True


charsetTests :: TestTree
charsetTests = testGroup "Muster.Internal.Charset"
  [ testProperty "==" propEq
  , testProperty "none" propNone
  , testProperty "any" propAny
  , testProperty "elem" propElem
  , insertTests
  , removeTests
  , intersectTests
  , testProperty "oneOf" propOneOf
  ]


propEq :: Charset -> Charset -> Bool
propEq ca cb = (ca == cb) /= (ca /= cb)


propNone :: Char -> Bool
propNone c = c `notElem` none


propAny :: Char -> Bool
propAny c = c `elem` any


propElem :: Char -> Charset -> Bool
propElem c cs = c `elem` cs /= c `notElem` cs


insertTests :: TestTree
insertTests = testGroup "insert"
  [ testProperty "inserts element" propInsert
  , testProperty "maintains uniqueness invariant" propInsertUniquenessInvariant
  ]


propInsert :: Char -> Charset -> Property
propInsert c cs =
  c `notElem` cs ==>
  c `elem` (c `insert` cs)


propInsertUniquenessInvariant :: Char -> Charset -> Property
propInsertUniquenessInvariant c cs =
  case c `insert` (c `insert` cs) of
    AnyOf cs' -> counterexample cs' $ L.length (L.elemIndices c cs') == 1
    NoneOf _ -> success


removeTests :: TestTree
removeTests = testGroup "remove"
  [ testProperty "removes element" propRemove
  , testProperty "maintains uniqueness invariant" propRemoveUniquenessInvariant
  ]


propRemove :: Char -> Charset -> Property
propRemove c cs =
  c `elem` cs ==>
  c `notElem` (c `remove` cs)


propRemoveUniquenessInvariant :: Char -> Charset -> Property
propRemoveUniquenessInvariant c cs =
  case c `remove` (c `remove` cs) of
    AnyOf _ -> success
    NoneOf cs' -> counterexample cs' $ L.length (L.elemIndices c cs') == 1


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
