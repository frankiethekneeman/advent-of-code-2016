module Parsing (
Direction(..),
parseDirections
) where

import ParsingUtils(lineByLine, charByChar)

data Direction = U | D | L | R

parseDirections :: String -> Maybe [[Direction]]
parseDirections = lineByLine $ charByChar parseDirection

parseDirection :: Char -> Maybe Direction
parseDirection 'U' = Just U
parseDirection 'D' = Just D
parseDirection 'L' = Just L
parseDirection 'R' = Just R
