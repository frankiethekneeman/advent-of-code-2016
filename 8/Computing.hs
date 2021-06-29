module Computing (
Pixel(..),
Screen,
runProgram
) where

import Parsing(Instruction(..))
import Seqs(update)
import Data.List(splitAt, transpose)

data Pixel = On | Off deriving (Eq)
type Screen = [[Pixel]]

startingScreen :: Screen
startingScreen = replicate 6 (replicate 50 Off)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = right ++ left
    where (left, right) = splitAt n' xs
          n' = l - (n `mod` l)
          l = length xs

apply :: Screen -> Instruction -> Screen
apply s (Rect width height) = modifiedRows ++ unmodifiedRows
    where modifiedRows = map ((replicate width On)++) upperRightQuadrant
          upperRightQuadrant = map (drop width) rowsToModify
          (rowsToModify, unmodifiedRows) = splitAt height s

apply s (RotRow idx dist) = update (rotate dist) idx s

apply s (RotCol idx dist) = transpose . rotcol . transpose $ s
    where rotcol = update (rotate dist) idx 

runProgram :: [Instruction] -> Screen
runProgram = foldl apply startingScreen
