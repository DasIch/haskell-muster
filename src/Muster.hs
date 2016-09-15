{-# LANGUAGE OverloadedStrings #-}

{-|

Muster is a library that provides composable regular expressions implemented
using derivatives.

= References

* Janusz A. Brzozowski (1964). Derivatives of Regular Expressions
* Scott Owens, John Reppy, Aaron Turon (2009). Regular-expression derivatives reexamined

-}
module Muster (
    Regex(Symbol),
    fromString,
    fromText,
    (<.>),
    (<|>),
    (<&>),
    many,
    many1,
    not,
    derivative,
    match
    ) where

import Prelude hiding (not)
import qualified Data.Bool as B

import Data.Text (Text)
import qualified Data.Text as T

import Muster.Internal.Regex


isNullable :: Regex -> Bool
isNullable None = False
isNullable Epsilon = True
isNullable (Symbol _) = False
isNullable (Concatenation l r) = isNullable l && isNullable r
isNullable (KleeneStar _) = True
isNullable (Or l r) = isNullable l || isNullable r
isNullable (And l r) = isNullable l && isNullable r
isNullable (Not o) = B.not $ isNullable o


-- | Produces an expression that matches all suffixes of the strings described
--   by the regular expression that start with the given character.
derivative :: Char -> Regex -> Regex
derivative _ None = None
derivative _ Epsilon = None
derivative x (Symbol y) = if x == y then Epsilon else None
derivative x (Concatenation l r)
  | isNullable l = (derivative x l <.> r) <|> derivative x r
  | otherwise    = derivative x l <.> r
derivative x (KleeneStar o) = derivative x o <.> many o
derivative x (Or l r) = derivative x l <|> derivative x r
derivative x (And l r) = derivative x l <&> derivative x r
derivative x (Not o) = not (derivative x o)


-- | Tests whether the regular expression matches the string.
match :: Regex -> Text -> Bool
match r text = case T.uncons text of
  Just (x, xs) -> match (derivative x r) xs
  Nothing -> isNullable r
