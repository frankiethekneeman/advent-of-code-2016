module Two where
import AoC (adventOfCode)
import Parsing(parseRuleset, Rules)
import Computing(State(..), increment, openingState, isFrozen)

import qualified Data.Map.Strict as Map

solve :: Rules -> Maybe Int
solve rules = foldl mult (Just 1) $ map (`Map.lookup` outputs) [0..2]
    where mult x y = (*) <$> x <*> y
          (State _ outputs) = exhaust rules $ openingState rules

exhaust :: Rules -> State -> State
exhaust rules state
    | isFrozen state = state
    | otherwise = exhaust rules $ increment rules state

main = adventOfCode parseRuleset solve "10" [("1", 30)]
