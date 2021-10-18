module Two where
import AoC (adventOfCode)
import Assembunny(readProgram, runProgram, readFrom, Arg (Register), Computer, writeRegister)

solve2 :: Computer -> Int
solve2 = readFrom (Register 'a') . runProgram . writeRegister 'c' 1

main = adventOfCode readProgram (Just . solve2) "12" []
