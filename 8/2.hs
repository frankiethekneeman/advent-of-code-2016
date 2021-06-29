module Two where
import AoC (adventOfCode)
import ParsingUtils(lineByLine)
import Parsing(parseInstruction, Instruction)
import Computing(runProgram, Pixel(..))
import Data.List(intercalate)

examples = []

pixelToChar :: Pixel -> Char
pixelToChar On = '#'
pixelToChar Off = '.'

visualizeScreen :: [Instruction] -> String
visualizeScreen = intercalate "\n" . map (map pixelToChar) . runProgram

main = adventOfCode (lineByLine parseInstruction) (Just . visualizeScreen) "8" examples
