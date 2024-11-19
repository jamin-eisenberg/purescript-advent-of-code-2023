module Day1 where

import Prelude

import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.List (List(..), catMaybes, head, last, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (fromNonEmpty)
import Data.String.CodeUnits (singleton)
import Data.Traversable (sequence, sum)
import Effect (Effect)
import Effect.Console (logShow)
import Main (run)
import Node.FS (FileFlags(..))
import Node.Process (argv)
import Parsing (Parser, liftMaybe)
import Parsing.Combinators (between, many1, optionMaybe, sepBy, sepBy1, sepEndBy)
import Parsing.Combinators.Array (many)
import Parsing.String (eof, string)
import Parsing.String.Basic (digit, letter, space)

main ∷ Effect Unit
main = do
  run 1 parser (note "missing num" <$> calc)

calc ∷ List (List Int) → Maybe Int
calc lines = do
  lineNums <- sequence (lineNum <$> lines)
  pure $ sum lineNums

lineNum ∷ List Int → Maybe Int
lineNum line = do
  tens <- head line
  ones <- last line
  pure $ tens * 10 + ones

parser :: Parser String (List (List Int))
parser = sepBy line (string "\n") <* eof

line :: Parser String (List Int)
line = do
  _ <- many letter
  ds <- (decimalDigit `sepEndBy` many letter)
  pure ds

decimalDigit ∷ Parser String Int
decimalDigit = do
  d <- digit
  liftMaybe (\_ -> "expected decimal digit") (fromString $ singleton d)