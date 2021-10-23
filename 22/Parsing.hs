module Parsing(
    Node(..),
    readDF,
) where

import Scanning((^&), scanInt, end, Parseable (..), get, grok)

whitespace = (==' ')
scanNode = "/dev/grid/node-x" ^& scanInt ^& "-y" ^& scanInt
scanSize = scanInt ^& "T"
scanStats = scanSize ^& whitespace ^& scanSize ^& whitespace ^& scanSize ^& whitespace ^& scanInt ^& "%" ^& end
scanLine = scanNode ^& whitespace ^& scanStats

data Node = Node {
    x, y, size, used :: Int,
    hasGoalData :: Bool
} deriving (Show, Eq, Ord)

instance Parseable Node where
    fromResult r = Node <$> x <*> y <*> size <*> used <*> Just False
        where x = fromInteger <$> get 0 r
              y = fromInteger <$> get 1 r
              size = fromInteger <$> get 3 r
              used = fromInteger <$> get 5 r

readDF :: String -> Maybe [Node]
readDF = mapM (grok scanLine) . drop 2 . lines
