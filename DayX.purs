module DayDAY_NUM where

import Prelude

import Effect (Effect)
import Main (run)
import Partial (crashWith)
import Data.Maybe (Maybe(..))

main ∷ Effect Unit
main = run DAY_NUM Nothing parser calc

calc lines = crashWith "calc not implemented"

parser = crashWith "parser not implemented"