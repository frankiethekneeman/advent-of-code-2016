module Two where
import AoC (adventOfCode)
import Parsing(Direction(..), parseDirections)
import Computing(computeCode)

testCases = [("1", "5"), ("2", "D"), ("3", "A"), ("4", "5"), ("5", "5DB3")]

addExcept :: [Int] -> Int -> Int -> Int
addExcept exceptions d pos = if elem pos exceptions then pos else pos + d

move :: Int -> Direction -> Int
move pos R = addExcept [1, 4, 9, 12, 13] 1 pos
move pos L = addExcept [1, 2, 5, 10, 13] (-1) pos
move pos D
    | elem pos [2,3,4,6,7,8] = pos + 4
    | elem pos [1, 11] = pos + 2
    | otherwise = pos
move pos U
    | elem pos [6,7,8,10,11,12] = pos - 4
    | elem pos [3, 13] = pos - 2
    | otherwise = pos


main = adventOfCode parseDirections (computeCode move) "2" testCases
