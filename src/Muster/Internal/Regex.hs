module Muster.Internal.Regex (
  Regex(..),
  fromString,
  fromText
  ) where

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
  deriving (Eq)


instance Show Regex where
  show None = "None"
  show Epsilon = "Epsilon"
  show (Symbol c) = show [c]
  show (Concatenation l r) =
    case gatherString (Concatenation l r) of
      ("", Just (Concatenation l' r')) -> "Concatenation (" ++ show l' ++ ") (" ++ show r' ++ ")"
      (string, Just r') -> "Concatenation " ++ show string ++ " (" ++ show r' ++ ")"
      (string, Nothing) -> show string
  show (KleeneStar r) = "KleeneStar (" ++ show r ++ ")"
  show (Or l r) = "Or (" ++ show l ++ ") (" ++ show r ++ ")"
  show (And l r) = "And (" ++ show l ++ ") (" ++ show r ++ ")"
  show (Not r) = "Not " ++ show r


gatherString :: Regex -> (String, Maybe Regex)
gatherString (Concatenation (Symbol lc) (Symbol rc)) = ([lc, rc], Nothing)
gatherString (Concatenation l (Symbol rc)) =
  case gatherString l of
    (string, Just l') -> (string, Just (Concatenation l' (Symbol rc)))
    (string, Nothing) -> (string ++ [rc], Nothing)
gatherString (Concatenation (Symbol lc) r) =
  (lc : string, regex)
  where (string, regex) = gatherString r
gatherString r = ("", Just r)


instance IsString Regex where
  fromString (c:cs) = foldl Concatenation (Symbol c) (fmap Symbol cs)
  fromString [] = Epsilon


fromText :: Text -> Regex
fromText = fromString . T.unpack
