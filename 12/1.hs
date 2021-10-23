module One where
import AoC (adventOfCode)
import Assembunny(readProgram, runProgram, readFrom, Arg (Register))

main = adventOfCode readProgram (Just . readFrom (Register 'a') . runProgram) "12" [("1", 42)]
