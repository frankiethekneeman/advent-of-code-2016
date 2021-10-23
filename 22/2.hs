module Two where
import AoC (adventOfCode, noOp)

import Parsing(readDF, Node(..))
import qualified AStar
import qualified Data.Set as Set

data State = State {
    goal, empty :: (Int, Int)
} deriving (Eq, Ord, Show)

between :: Int -> Int -> Int -> Bool
between min max subj = min <= subj && max >= subj

d :: (Int, Int) -> (Int, Int) -> Int
d (x1, y1) (x2, y2) = abs (x1 - x2) + (abs y1 - y2)

step :: Int -> Int -> Set.Set (Int, Int) -> State -> [State]
step maxX maxY walls (State goal empty) = map move notWalls
    where move p = if p == goal then State empty p else State goal p
          notWalls = filter (`Set.notMember` walls) inBounds
          inBounds = filter (between 0 maxX . fst) . filter (between 0 maxY . snd) $ neighbors
          neighbors = [(ex + 1, ey), (ex - 1, ey), (ex, ey + 1), (ex, ey - 1)]
          (ex, ey) = empty

dist :: State -> Int
dist (State (x, y) (ex, ey)) = goalDist
    where goalDist = x + y
          emptyDist = abs (ex - x') + abs (ey - y')
          x' = min 0 (x - 1)
          y' = min 0 (y - 1)

getEmpty :: [Node] -> (Int, Int)
getEmpty nodes = (x empty, y empty)
    where empty = head . filter ((==0).used) $ nodes

f :: [Node] -> Maybe Int
f nodes = subtract 1 . length <$> path
    where path = AStar.uniformWeights dist moves start
          moves = step maxX maxY walls
          start = State (maxX, 0) $ getEmpty nodes
          walls = Set.fromList . map toPair . filter ((>200).size) $ nodes
          maxX = maximum . map x $ nodes
          maxY = maximum . map y $ nodes
          toPair n = (x n, y n)

main = adventOfCode readDF f "22" [("1", 7)]
