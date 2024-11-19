module DayDAY_NUM where

import Prelude

import Effect (Effect)
import Main (run)
import Partial (crashWith)

main ∷ Effect Unit
main = run DAY_NUM parser calc

calc lines = crashWith "calc not implemented"

parser = crashWith "parser not implemented"