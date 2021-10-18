module Parsing (
    Maze(..),
    parseMaze
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char(digitToInt, isDigit)

data Maze = Maze {
    walls :: Set.Set (Int, Int),
    locations :: Map.Map Int (Int, Int)
} deriving (Show)

matrix :: [[(Int, Int)]]
matrix = [[(i, j) | j <- [0..]] | i <- [0..]]

parseMaze :: String -> Maze
parseMaze input = Maze walls locations
    where walls = Set.fromList . map snd . filter ((=='#').fst) $ coordinates
          locations = Map.fromList . map parseLoc . filter (isDigit.fst) $ coordinates
          coordinates = concat $ zipWith (zipWith (,)) (lines input) matrix
          parseLoc (dig, coord) = (digitToInt dig, coord)

