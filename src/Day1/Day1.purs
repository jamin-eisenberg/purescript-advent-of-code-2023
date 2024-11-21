module Day1 where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.List (List(..), catMaybes, head, last, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (fromNonEmpty)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (singleton)
import Data.Traversable (sequence, sum)
import Debug as Debug
import Effect (Effect)
import Effect.Console (logShow)
import Main (run)
import Node.FS (FileFlags(..))
import Node.Process (argv)
import Parsing (Parser, liftMaybe)
import Parsing.Combinators (between, choice, many1, optionMaybe, sepBy, sepBy1, sepEndBy)
import Parsing.Combinators.Array (many)
import Parsing.String (eof, string)
import Parsing.String.Basic (digit, letter, space)

main ∷ Effect Unit
main = do
  run 1 (Just expandOverlapping) parser (note "missing num" <$> calc)

expandOverlapping ∷ String → String
expandOverlapping =
  replaceAll (Pattern "twone") (Replacement "twoone")
    >>> replaceAll (Pattern "oneight") (Replacement "oneeight")
    >>> replaceAll (Pattern "eightwo") (Replacement "eighttwo")
    >>> replaceAll (Pattern "eighthree") (Replacement "eightthree")
    >>> replaceAll (Pattern "nineight") (Replacement "nineeight")
    >>> replaceAll (Pattern "one") (Replacement "1")
    >>> replaceAll (Pattern "two") (Replacement "2")
    >>> replaceAll (Pattern "three") (Replacement "3")
    >>> replaceAll (Pattern "four") (Replacement "4")
    >>> replaceAll (Pattern "five") (Replacement "5")
    >>> replaceAll (Pattern "six") (Replacement "6")
    >>> replaceAll (Pattern "seven") (Replacement "7")
    >>> replaceAll (Pattern "eight") (Replacement "8")
    >>> replaceAll (Pattern "nine") (Replacement "9")
    >>> replaceAll (Pattern "zero") (Replacement "0")

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

-- digitString ∷ Parser String Char
-- digitString = choice
--   [ string "one" $> '1'
--   , string "two" $> '2'
--   , string "three" $> '3'
--   , string "four" $> '4'
--   , string "five" $> '5'
--   , string "six" $> '6'
--   , string "seven" $> '7'
--   , string "eight" $> '8'
--   , string "nine" $> '9'
--   , string "zero" $> '0'
--   ]