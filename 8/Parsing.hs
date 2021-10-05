module Parsing (
Instruction (..),
parseInstruction,
) where
import Text.Read(readMaybe)
import Seqs(splitOn, forceLength)
import Data.List(isPrefixOf, stripPrefix)

data Instruction = Rect Int Int | RotCol Int Int | RotRow Int Int deriving (Show)

--CONSTANTS
rect_pre = "rect "
col_pre = "rotate column x="
row_pre = "rotate row y="

parseInstruction :: String -> Maybe Instruction
parseInstruction s
    | isPrefixOf rect_pre s = parseRect s
    | isPrefixOf col_pre s = parseCol s
    | isPrefixOf row_pre s = parseRow s
    | otherwise = Nothing

parseRect :: String -> Maybe Instruction
parseRect s = Rect <$> width <*> height
    where width = readMaybe . (!! 0) =<< nums
          height = readMaybe . (!! 1) =<< nums
          nums = forceLength 2 . splitOn "x" =<< spec
          spec = stripPrefix rect_pre s

parseCol :: String -> Maybe Instruction
parseCol s = RotCol <$> idx <*> dist
    where idx = readMaybe . (!! 0) =<< nums
          dist = readMaybe . (!! 1) =<< nums
          nums = forceLength 2 . splitOn " by " =<< spec
          spec = stripPrefix col_pre s

parseRow :: String -> Maybe Instruction
parseRow s = RotRow <$> idx <*> dist
    where idx = readMaybe . (!! 0) =<< nums
          dist = readMaybe . (!! 1) =<< nums
          nums = forceLength 2 . splitOn " by " =<< spec
          spec = stripPrefix row_pre s
