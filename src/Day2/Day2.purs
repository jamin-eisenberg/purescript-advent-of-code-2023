module Day2 where

import Prelude

import Data.Array (all, concat, concatMap, filter, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (maximum, product, sum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Debug (spyWith)
import Effect (Effect)
import Main (run)
import Parsing (Parser)
import Parsing.Combinators (choice, sepBy)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal)
import Partial (crashWith)

main ∷ Effect Unit
main = run 2 Nothing parser calc

calc ∷ Array { id ∷ Int, rounds ∷ Array (Array { color :: Color, count :: Int }) } → Either String Int
calc lines =
  let
    _ = spyWith "" show lines
  in
    -- filter possible lines
    --   # map _.id
    --   # sum
    --   # Right
    lines
      # map power
      # sum
      # Right

data Color = Red | Green | Blue

derive instance Eq Color

derive instance Generic Color _
instance Show Color where
  show = genericShow

type Draw = { color :: Color, count :: Int }
type Round = Array Draw
type Game = { id :: Int, rounds :: Array Round }

power :: Game -> Int
power { rounds } =
  let
    draws = concat rounds
    maxOf c = filter (_.color >>> eq c) draws
      # map _.count
      # maximum
      # fromMaybe 0
  in
    maxOf Red * maxOf Green * maxOf Blue

possible :: Game -> Boolean
possible { rounds } =
  all
    ( all
        ( \{ color, count } -> count <= case color of
            Red -> 12
            Green -> 13
            Blue -> 14
        )
    )
    rounds

parser :: Parser String (Array Game)
parser = game `sepBy` string "\n" <#> fromFoldable

game :: Parser String Game
game = do
  _ <- string "Game "
  id <- intDecimal
  _ <- string ": "
  rounds <- round `sepBy` string "; " <#> fromFoldable
  pure { id, rounds }

round :: Parser String Round
round = draw `sepBy` string ", " <#> fromFoldable

draw :: Parser String Draw
draw = do
  count <- intDecimal
  _ <- string " "
  color <- choice [ string "red" $> Red, string "green" $> Green, string "blue" $> Blue ]
  pure { count, color }