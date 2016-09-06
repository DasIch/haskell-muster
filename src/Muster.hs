{-# LANGUAGE OverloadedStrings #-}

{-|

Muster is a library that provides composable regular expressions implemented
using derivatives.

= References

* Janusz A. Brzozowski (1964). Derivatives of Regular Expressions
* Scott Owens, John Reppy, Aaron Turon (2009). Regular-expression derivatives reexamined

-}
module Muster (
    Regex(..),
    fromString,
    fromText,
    (<.>),
    (<|>),
    (<&>),
    many,
    many1,
    derivative,
    match
    ) where

import GHC.Exts (IsString(..))

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck


data Regex
  -- | Corresponds to the empty set.
  = None

  -- | Corresponds to the set containing only the empty string.
  | Epsilon
  | Symbol Char
  | Concatenation Regex Regex

  -- | Zero or more occurences of the regex.
  | KleeneStar Regex

  -- | Union of the strings described by the left and right expressions.
  | Or Regex Regex

  -- | Intersection of the strings described by the left and right expressions.
  --   In other words a string matching this regular expression has to match
  --   both the left and right expression.
  | And Regex Regex
  | Not Regex
  deriving (Eq)


instance Show Regex where
  show None = "None"
  show Epsilon = "Epsilon"
  show (Symbol x) = show [x]
  show (Concatenation l r) =
    if string == ""
      then "Concatenation (" ++ show l ++ ") (" ++ show r ++ ")"
      else case regex of
        Just r -> "Concatenation " ++ show string ++ " (" ++ show r ++ ")"
        Nothing -> show string
    where
      (string, regex) = gatherString (Concatenation l r)
      gatherString :: Regex -> (String, Maybe Regex)
      gatherString (Concatenation (Symbol l) (Symbol r)) = ([l, r], Nothing)
      gatherString (Concatenation l (Symbol c)) =
        case regex of
            Just _ -> ("", Just (Concatenation l (Symbol c)))
            Nothing -> (string ++ [c], Nothing)
        where (string, regex) = gatherString l
      gatherString (Concatenation (Symbol c) r) = (c : string, regex)
        where (string, regex) = gatherString r
      gatherString r = ("", Just r)

  show (KleeneStar r) = "KleeneStar (" ++ show r ++ ")"
  show (Or l r) = "Or (" ++ show l ++ ") (" ++ show r ++ ")"
  show (And l r) = "And (" ++ show l ++ ") (" ++ show r ++ ")"
  show (Not r) = "Not " ++ show r


instance IsString Regex where
  fromString (x:xs) = foldl Concatenation (Symbol x) (fmap Symbol xs)
  fromString _ = Epsilon

fromText :: Text -> Regex
fromText = fromString . T.unpack


infixl 5 <.>

-- | 'Concatenation' operator.
(<.>) :: Regex -> Regex -> Regex
(<.>) = Concatenation


infixl 6 <|>, <&>

-- | 'Or' operator.
(<|>) :: Regex -> Regex -> Regex
(<|>) = Or

-- | 'And' operator.
(<&>) :: Regex -> Regex -> Regex
(<&>) = And


-- | Alias for the 'KleeneStar' constructor.
many :: Regex -> Regex
many = KleeneStar


-- | One or more occurrences.
many1 :: Regex -> Regex
many1 r = Concatenation r (KleeneStar r)


isNullable :: Regex -> Bool
isNullable None = False
isNullable Epsilon = True
isNullable (Symbol _) = False
isNullable (Concatenation l r) = isNullable l && isNullable r
isNullable (KleeneStar _) = True
isNullable (Or l r) = isNullable l || isNullable r
isNullable (And l r) = isNullable l && isNullable r
isNullable (Not o) = not $ isNullable o


derivative' :: Char -> Regex -> Regex
derivative' _ None = None
derivative' _ Epsilon = None
derivative' x (Symbol y) = if x == y then Epsilon else None
derivative' x (Concatenation l r) =
  if isNullable l
  then (derivative' x l <.> r) <|> derivative' x r
  else derivative' x l <.> r
derivative' x (KleeneStar o) = derivative' x o <.> KleeneStar o
derivative' x (Or l r) = derivative' x l <|> derivative' x r
derivative' x (And l r) = derivative' x l <&> derivative' x r
derivative' x (Not o) = Not (derivative' x o)


normalize :: Regex -> Regex
normalize (Concatenation None _) = None
normalize (Concatenation Epsilon r) = r
normalize (KleeneStar o) = KleeneStar (normalize o)
normalize (Or None r) = r
normalize (Or l None) = l
normalize (Or l r) =
  if l' == r' then l' else Or l' r'
  where l' = normalize l
        r' = normalize r
normalize (And l r) =
  if l' == r' then l' else And l' r'
  where l' = normalize l
        r' = normalize r
normalize (Not o) = Not (normalize o)
normalize r = r


-- | Produces an expression that matches all suffixes of the strings described
--   by the regular expression that start with the given character.
derivative :: Char -> Regex -> Regex
derivative c = normalize . derivative' c


-- | Tests whether the regular expression matches the string.
match :: Regex -> Text -> Bool
match r text = case T.uncons text of
  Just (x, xs) -> match (derivative x r) xs
  Nothing -> isNullable r
