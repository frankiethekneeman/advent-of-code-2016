module Parsing (
    Direction(..),
    Instruction(Instruction),
    toInstructions
) where

import Seqs (splitOn)
import Text.Read (readMaybe)

data Direction = L | R deriving (Show)
data Instruction = Instruction { dir :: Direction, dist :: Int} deriving (Show)

toInstructions :: String -> Maybe [Instruction]
toInstructions line = mapM parseInstruction directions
    where directions = splitOn ", " line

parseInstruction :: String -> Maybe Instruction
parseInstruction "" = Nothing
parseInstruction (dir:dist) = Instruction `fmap` dir' <*> dist'
    where dir' = parseDirection dir
          dist' = readMaybe dist :: Maybe Int

parseDirection :: Char -> Maybe Direction
parseDirection 'R' = Just R
parseDirection 'L' = Just L
parseDirection _ = Nothing
