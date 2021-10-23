module One where

import AoC (adventOfCode)
import ParsingUtils(lineByLine)
import Parsing(Triangle, fromList)
import Computing(countValid)
import Text.Read(readMaybe)

examples = [("1", 0), ("2", 0), ("3", 2), ("4", 3)]

parseTriangle :: String -> Maybe Triangle
parseTriangle input = fromList =<< sides
    where sides = mapM (readMaybe :: String -> Maybe Int) $ words input

main = adventOfCode (lineByLine parseTriangle) (Just . countValid) "3" examples
