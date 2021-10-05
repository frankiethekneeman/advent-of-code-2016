module Two where
import AoC (adventOfCode)
import Parsing(parseInput, readRegister, Computer, writeRegister)
import Computing(runProgram)

solve2 :: Computer -> Int
solve2 = readRegister 'a' . runProgram . writeRegister 'c' 1

main = adventOfCode parseInput (Just . solve2) "12" []
