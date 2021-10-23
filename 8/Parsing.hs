module Parsing (
    Instruction (..),
    parseInstruction,
) where

import Text.Read(readMaybe)
import Seqs(splitOn, forceLength)
import Data.List(isPrefixOf, stripPrefix)

data Instruction = Rect Int Int | RotCol Int Int | RotRow Int Int deriving (Show)

--CONSTANTS
rectPre = "rect "
colPre = "rotate column x="
rowPre = "rotate row y="

parseInstruction :: String -> Maybe Instruction
parseInstruction s
    | rectPre `isPrefixOf` s = parseRect s
    | colPre `isPrefixOf` s = parseCol s
    | rowPre `isPrefixOf` s = parseRow s
    | otherwise = Nothing

parseRect :: String -> Maybe Instruction
parseRect = parseIns Rect rectPre "x"

parseCol :: String -> Maybe Instruction
parseCol = parseIns RotCol colPre " by "

parseRow :: String -> Maybe Instruction
parseRow = parseIns RotRow rowPre " by "

parseIns :: (Int -> Int -> t) -> String -> String -> String -> Maybe t
parseIns cons pre delim s = cons <$> first <*> second
    where first = readMaybe . head =<< nums
          second = readMaybe . (!! 1) =<< nums
          nums = forceLength 2 . splitOn delim =<< spec
          spec = stripPrefix pre s
