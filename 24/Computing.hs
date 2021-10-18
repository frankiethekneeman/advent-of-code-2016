module Computing(
    buildAdjacencyList,
    State(..),
    countRemaining,
    next,
) where
import Parsing(Maze(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified AStar

type Adj = Map.Map (Int, Int) Int

data State = State {
    curr :: Int,
    seen :: Set.Set Int
} deriving (Eq, Ord, Show)

-- Building Adjacency lists (reducing the graph)
manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = xDist + yDist
    where xDist = abs (x1 - x2)
          yDist = abs (y1 - y2)

neighbors :: Set.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbors walls (x, y) = filter ((flip Set.notMember) walls) adjacent
    where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findDistance :: Maze -> Int -> Int -> Maybe Int
findDistance (Maze walls locations) start end = (subtract 1 . length) <$> path
    where path = AStar.uniformWeights dist next origin
          dist = manhattanDistance target
          next = neighbors walls'
          walls' = Set.union walls disallowedLocations
          disallowedLocations = Set.fromList . Map.elems . Map.delete start . Map.delete end $ locations
          origin = locations Map.! start
          target = locations Map.! end

buildAdjacencyList :: Maze -> Adj
buildAdjacencyList maze = Map.fromList dists
    where dists = concat . map toBothDirections $ toFind
          toBothDirections (l, r, Just d) = [((l, r), d), ((r, l), d)]
          toBothDirections (l, r, Nothing) = []
          toFind = [(l, r, findDistance maze l r) | l <- locii, r <- locii, l < r]
          locii = Map.keys . locations $ maze

-- AStar Helper Functions
travel :: State -> Int -> State
travel (State curr seen) next = State next seen'
    where seen' = Set.insert next seen

next :: Adj -> State -> [AStar.Step State]
next adj state = map toStep nexts
    where toStep ((_, next), d) = AStar.Step d $ travel state next
          nexts = Map.toList $ Map.filterWithKey startsHere adj
          startsHere (start, _) _ = start == curr state

countRemaining :: Set.Set Int -> State -> Int
countRemaining toSee (State _ seen) = Set.size $ Set.difference toSee seen
