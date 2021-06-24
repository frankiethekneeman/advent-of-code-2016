module Computing (
Heading(..),
Position(..),
Location(..),
applyAllIns,
distance
) where
import Parsing (Direction(..), Instruction(Instruction))

data Heading = North | South | East | West
data Position = Position Int Int deriving Eq
data Location = Location Heading Position

applyAllIns :: Location -> [Instruction] -> [Location]
applyAllIns start ins = concat $ scanl (applyIns . last) [start] ins

distance :: Location -> Int
distance (Location _ (Position x y)) = abs x + abs y

turn :: Heading -> Direction -> Heading
turn North L = West
turn West L = South
turn South L = East
turn East L = North
turn North R = East
turn East R = South
turn South R = West
turn West R = North

move :: Position -> Heading -> Position
move (Position x y) North = (Position x (y + 1))
move (Position x y) South = (Position x (y - 1))
move (Position x y) East = (Position (x + 1) y)
move (Position x y) West = (Position (x - 1) y)

moveMany :: Position -> Heading -> Int -> [Position]
moveMany _ _ 0 = []
moveMany pos heading count = pos':subsequent
    where pos' = move pos heading
          subsequent = moveMany pos' heading count'
          count' = count - 1

applyIns :: Location -> Instruction -> [Location]
applyIns (Location heading pos) (Instruction dir dist) = map (Location heading') $ positions
    where heading' = turn heading dir
          positions = moveMany pos heading' dist

instance Ord Position where
    compare (Position x1 y1) (Position x2 y2)
        | x1 == x2 = compare y1 y2
        | otherwise = compare x1 x2
