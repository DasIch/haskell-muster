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
