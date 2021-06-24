import Parsing (Instruction(Instruction), toInstructions)
import Computing (applyAllIns, Position(..), Heading(..), Location(..), distance)
import AoC (adventOfCode)

testCases = [("1", 5), ("2", 2), ("3", 12)]

solve :: [Instruction] -> Int
solve instructions = distance $ last positions 
    where positions = applyAllIns (Location North (Position 0 0)) instructions

main = adventOfCode toInstructions (Just . solve) "1" testCases
