import Parsing (Instruction(Instruction), toInstructions)
import Computing (applyAllIns, Position(..), Heading(..), Location(..), distance)
import AoC (adventOfCode)

import qualified Data.Set as Set

testCases = [("4", 4)]

solve :: [Instruction] -> Maybe Int
solve instructions = fmap distance $ firstRevisited Set.empty positions 
    where positions = applyAllIns (Location North (Position 0 0)) instructions

firstRevisited :: Set.Set Position -> [Location] -> Maybe Location
firstRevisited _ [] = Nothing
firstRevisited seen (curr:rest)
    | Set.member pos seen = Just curr
    | otherwise = firstRevisited seen' rest
    where (Location _ pos) = curr
          seen' = Set.insert pos seen

main = adventOfCode toInstructions solve "1" testCases
