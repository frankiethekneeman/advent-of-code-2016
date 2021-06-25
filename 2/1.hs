module One where
import AoC (adventOfCode)
import Parsing(Direction(..), parseDirections)
import Computing(computeCode)

testCases = [("1", "1"), ("2", "9"), ("3", "4"), ("4", "5"), ("5", "1985")]

move :: Int -> Direction -> Int
move x U = if x > 3 then x - 3 else x
move x D = if x < 7 then x + 3 else x
move x L = if x `mod` 3 == 1 then x else x - 1
move x R = if x `mod` 3 == 0 then x else x + 1

main = adventOfCode parseDirections (computeCode move) "2" testCases
