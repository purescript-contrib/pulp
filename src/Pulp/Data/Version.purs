module Pulp.Data.Version
  ( Version(..)
  , runVersion
  , showVersion
  , parseVersion
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Data.Int (fromNumber)
import Data.String (fromCharArray, toCharArray, joinWith)
import Data.List (List(), toList, fromList, some, null)
import Data.Function (on)
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Control.Apply ((<*))
import Control.Monad (unless)
import Control.Monad.State.Class (get)
import Global (readInt)
import Text.Parsing.Parser (Parser(), PState(..), ParseError(..), runParser, fail)
import Text.Parsing.Parser.Token (when)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.Pos (Position(), initialPos)

import Pulp.System.FFI

newtype Version = Version (List Int)

runVersion :: Version -> List Int
runVersion (Version xs) = xs

showVersion :: Version -> String
showVersion = joinWith "." <<< fromList <<< map show <<< runVersion

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

int :: Parser (List Char) Int
int = (fromJust <<< parseInt <<< fromCharArray <<< fromList) <$> some (when lieAboutPos isDigit)

lieAboutPos :: forall a. a -> Position
lieAboutPos = const initialPos

dot :: Parser (List Char) Unit
dot = void $ when lieAboutPos (== '.')

parseInt :: String -> Maybe Int
parseInt = fromNumber <<< readInt 10

version :: Parser (List Char) Version
version = Version <$> (int `sepBy` dot) <* eof

parseVersion :: String -> Either ParseError Version
parseVersion = flip runParser version <<< toList <<< toCharArray

eof :: forall a. Parser (List a) Unit
eof =
  get >>= \(input :: List a) ->
    unless (null input) (fail "expected eof")

instance eqVersion :: Eq Version where
  eq = eq `on` runVersion

instance ordVersion :: Ord Version where
  compare = compare `on` runVersion

instance _showVersion :: Show Version where
  show (Version xs) = "(Version " <> show xs <> ")"
