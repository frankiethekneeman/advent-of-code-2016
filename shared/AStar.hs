module AStar (
    Step(..),
    uniformWeights,
    weighted,
) where

import qualified Data.Set as Set
import qualified MinPQueue as Queue
import Debug.Trace(trace)

data Step a = Step Int a
data Path a = Path Int [a]

type DistF a = a -> Int
type NeighborsF a = a -> [a]
type Paths a = Queue.MinPQueue Int [a]
type StepsF a = a -> [Step a]
type WeightedPaths a = Queue.MinPQueue Int (Path a)


uniformWeights :: Ord a => DistF a -> NeighborsF a -> a -> Maybe [a]
uniformWeights dist neighbors start = snd <$> weightedSearch dist next Set.empty (startPath, Queue.empty)
    where next = map (Step 1) . neighbors
          startPath = Path 0 [start]

weighted :: Ord a => DistF a -> StepsF a -> a -> Maybe (Int, [a])
weighted dist next start = weightedSearch dist next Set.empty (Path 0 [start], Queue.empty)

--astar dist neighbors (Queue.singleton (1 + dist start) [start]) Set.empty

vertex :: Step a -> a
vertex (Step _ x) = x

weightedSearch :: Ord a => DistF a -> StepsF a -> Set.Set a -> (Path a, WeightedPaths a) -> Maybe (Int, [a])
weightedSearch _ _ _ (Path _ [], _) = Nothing
weightedSearch dist next seen (Path cost path, paths)
    | dist curr == 0 = Just (cost, path)
    | Set.member curr seen = Queue.pop paths >>= weightedSearch dist next seen
    | otherwise = Queue.pop paths' >>= weightedSearch dist next seen'
    where curr = head path
          paths' = Queue.insertAll paths steps
          steps = map prep . filter (not.alreadySeen.vertex) $ next curr
          prep (Step weight vertex) = (cost + weight + dist vertex, Path (cost + weight) (vertex:path))
          alreadySeen = flip Set.member seen'
          seen' = Set.insert curr seen

