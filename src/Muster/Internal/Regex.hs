module Muster.Internal.Regex (
  Regex(..),
  fromString,
  fromText,
  (<.>), (<|>), (<&>),
  many, many1,
  not
  ) where

import Prelude hiding (not)
import GHC.Exts (IsString (..))

import Data.Text (Text)
import qualified Data.Text as T


data Regex
  = None
  | Epsilon
  | Symbol Char
  | Concatenation Regex Regex
  | KleeneStar Regex
  | Or Regex Regex
  | And Regex Regex
  | Not Regex
  deriving (Eq, Ord)


appPrec :: Int
appPrec = 10

instance Show Regex where
  showsPrec _ None = showString "None"
  showsPrec _ Epsilon = showString "Epsilon"
  showsPrec _ (Symbol c) = showString (show [c])
  showsPrec p (Concatenation l r) =
    case gatherString (Concatenation l r) of
      ("", Just _) ->
        showParen (p > appPrec) $
        showString "Concatenation " . showsPrec (appPrec + 1) l . showChar ' ' .
                                      showsPrec (appPrec + 1) r
      (string, Just r') ->
        showParen (p > appPrec) $
        showString "Concatenation " . showString (show string) . showChar ' ' .
                                      showsPrec (appPrec + 1) r'
      (string, Nothing) -> showString (show string)
  showsPrec p (KleeneStar r) =
    showParen (p > appPrec) $
    showString "KleeneStar " . showsPrec (appPrec + 1) r
  showsPrec p (Or l r) =
    showParen (p > appPrec) $
    showString "Or " . showsPrec (appPrec + 1) l . showChar ' ' .
                       showsPrec (appPrec + 1) r
  showsPrec p (And l r) =
    showParen (p > appPrec) $
    showString "And " . showsPrec (appPrec + 1) l . showChar ' ' .
                        showsPrec (appPrec + 1) r
  showsPrec p (Not r) =
    showParen (p > appPrec) $
    showString "Not " . showsPrec (appPrec + 1) r


gatherString :: Regex -> (String, Maybe Regex)
gatherString (Concatenation (Symbol lc) (Symbol rc)) = ([lc, rc], Nothing)
gatherString (Concatenation l (Symbol rc)) =
  case gatherString l of
    (string, Just l') -> (string, Just (l' <.> Symbol rc))
    (string, Nothing) -> (string ++ [rc], Nothing)
gatherString (Concatenation (Symbol lc) r) =
  (lc : string, regex)
  where (string, regex) = gatherString r
gatherString r = ("", Just r)


instance IsString Regex where
  fromString (c:cs) = foldl (<.>) (Symbol c) (fmap Symbol cs)
  fromString [] = Epsilon


fromText :: Text -> Regex
fromText = fromString . T.unpack


infix 5 <.>

(<.>) :: Regex -> Regex -> Regex
(Concatenation r s) <.> t = r <.> (s <.> t)
None <.> _ = None
_ <.> None = None
Epsilon <.> r = r
r <.> Epsilon = r
r <.> s = Concatenation r s


infix 6 <|>

(<|>) :: Regex -> Regex -> Regex
(Or r s) <|> t = r <|> (s <|> t)
None <|> r = r
r <|> None = r
(Not None) <|> r = Not None
r <|> (Not None) = Not None
r <|> s
  | r == s = r
  | r < s = Or r s
  | r > s = Or s r


infix 6 <&>

(<&>) :: Regex -> Regex -> Regex
(And r s) <&> t = r <&> (s <&> t)
None <&> r = None
r <&> None = None
(Not None) <&> r = r
r <&> (Not None) = r
r <&> s
  | r == s = r
  | r < s = And r s
  | r > s = And s r


many :: Regex -> Regex
many (KleeneStar r) = KleeneStar r
many Epsilon = Epsilon
many None = Epsilon
many r = KleeneStar r


many1 :: Regex -> Regex
many1 r = r <.> many r


not :: Regex -> Regex
not (Not r) = Not r
not r = Not r
