module One where
import AoC (adventOfCode)
import Parsing(parseRuleset, Rules)
import Computing(State(..), increment, openingState, isFrozen)

import Seqs(safeHead)
import MonadUtils(orElseTry)
import qualified Data.Map.Strict as Map

solve :: Int -> Int -> Rules -> Maybe Int
solve low high rules = findComparator low high rules $ openingState rules

extractComparator :: Int -> Int -> State -> Maybe Int
extractComparator low high (State bots _) = safeHead $ map fst $ Map.toList $ Map.filter (== [low, high]) bots

findComparator :: Int -> Int -> Rules -> State -> Maybe Int
findComparator low high rules state
    | high < low = findComparator high low rules state
    | isFrozen state = Nothing
    | otherwise = found `orElseTry` recurse
        where found = extractComparator low high state
              recurse = findComparator low high rules state'
              state' = increment rules state

main = adventOfCode parseRuleset (solve 17 61) "10" []
