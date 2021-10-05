module Parsing (
    parseInput
) where

import Text.Read(readMaybe)

parseInput :: String -> Maybe Int
parseInput = readMaybe
