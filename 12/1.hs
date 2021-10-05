module One where
import AoC (adventOfCode)
import Parsing(parseInput, readRegister)
import Computing(runProgram)


main = adventOfCode parseInput (Just . readRegister 'a' . runProgram) "12" [("1", 42)]

