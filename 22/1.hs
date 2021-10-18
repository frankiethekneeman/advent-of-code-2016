module One where

import AoC (adventOfCode)
import Parsing(readDF, Node)
import Computing(viablePairs)

countViablePairs :: [Node] -> Maybe Int
countViablePairs = Just . length . viablePairs

main = adventOfCode readDF countViablePairs "22" []
