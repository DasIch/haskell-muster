module Muster.Internal.Charset
  ( Charset(..)
  , none
  , any
  , elem
  , notElem
  , insert
  , remove
  , intersect
  , oneOf
  ) where

import Prelude hiding (any, elem, notElem)
import qualified Data.List as L
import Data.Maybe

import Test.QuickCheck


data Charset = AnyOf String
             | NoneOf String
             deriving (Show)


instance Eq Charset where
  (AnyOf xs) == (AnyOf ys) = xs == ys
  (AnyOf xs) == (NoneOf ys) =
    case AnyOf xs `intersect` NoneOf ys of
      AnyOf xs' -> xs == xs'
      NoneOf ys' -> ys == ys'
  (NoneOf xs) == (NoneOf ys) = xs == ys


instance Arbitrary Charset where
  arbitrary = oneof
    [ AnyOf . L.sort . L.nub <$> arbitrary
    , NoneOf . L.sort . L.nub <$> arbitrary
    ]
  shrink (AnyOf xs)  = map AnyOf $ shrink xs
  shrink (NoneOf xs) = map NoneOf $ shrink xs


none :: Charset
none = AnyOf []


any :: Charset
any = NoneOf []


elem :: Char -> Charset -> Bool
elem c (AnyOf cs) = c `L.elem` cs
elem c (NoneOf cs) = c `L.notElem` cs


notElem :: Char -> Charset -> Bool
notElem c cs = not (c `elem` cs)


insert :: Char -> Charset -> Charset
insert c (AnyOf cs) = AnyOf (L.insert c cs)
insert c (NoneOf cs) = NoneOf (L.delete c cs)


remove :: Char -> Charset -> Charset
remove c (AnyOf cs) = AnyOf (L.delete c cs)
remove c (NoneOf cs) = NoneOf (L.insert c cs)


intersect :: Charset -> Charset -> Charset
intersect (AnyOf xs) (AnyOf ys) = AnyOf $ xs `L.intersect` ys
intersect (AnyOf xs) (NoneOf ys) = AnyOf $ filter (`elem` NoneOf ys) xs
intersect (NoneOf xs) (AnyOf ys) = AnyOf ys `intersect` NoneOf xs
intersect (NoneOf xs) (NoneOf ys) = NoneOf . L.sort $ xs `L.union` ys


oneOf :: Charset -> Maybe Char
oneOf (AnyOf xs) = listToMaybe xs
oneOf (NoneOf xs) = listToMaybe $ filter (`elem` NoneOf xs) [minBound..maxBound]
