module One where
import AoC (adventOfCode)
import ParsingUtils(lineByLine)
import Parsing(parseInstruction, Instruction)
import Computing(runProgram, Pixel(..))

examples = [("1", 6), ("2", 9), ("3", 9), ("4", 4), ("5", 4)]

countLitPixels :: [Instruction] -> Int
countLitPixels = length . filter (==On) . concat . runProgram

main = adventOfCode (lineByLine parseInstruction) (Just . countLitPixels) "8" examples
