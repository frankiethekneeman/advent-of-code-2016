module Two where
import AoC (adventOfCode, noOp)
import Parsing(Triangle, fromList)
import Computing(countValid)
import Text.Read(readMaybe)
import Data.List(transpose)
import Seqs(chunksOf)
import Control.Monad((<=<))

examples = [("1", 3), ("2", 3), ("3", 5), ("4", 6)]

readInts :: String -> Maybe [[Int]]
readInts = sequence . map lineToInts . lines
    where lineToInts = sequence . map (readMaybe :: String -> Maybe Int) . words

toTriangles :: [[Int]] -> Maybe [Triangle]
toTriangles = sequence . map fromList . transpose

parseVerticalTriangles :: String -> Maybe [Triangle]
parseVerticalTriangles = fmap concat . sequence . map toTriangles <=< fmap (chunksOf 3) . readInts

main = adventOfCode parseVerticalTriangles (Just . countValid) "3" examples
