module AStar (
    search
) where

import qualified Data.Set as Set
import qualified MinPQueue as Queue

type DistF a = a -> Int
type NeighborsF a = a -> [a]
type Paths a = Queue.MinPQueue Int [a]

search :: Ord a => DistF a -> NeighborsF a -> a -> Maybe [a]
search dist neighbors start = astar dist neighbors (Queue.singleton (1 + dist start) [start]) Set.empty

astar :: Ord a => DistF a -> NeighborsF a -> Paths a -> Set.Set a -> Maybe [a]
astar dist neighbors paths seen = Queue.pop paths >>= step dist neighbors seen

step :: Ord a => DistF a -> NeighborsF a -> Set.Set a -> ([a], Paths a) -> Maybe [a]
step _ _ _ ([], _) = Nothing
step dist neighbors seen (path@(curr:_), paths) = if dist curr == 0 then Just path
    else if Set.member curr seen then astar dist neighbors paths seen
    else astar dist neighbors paths' seen'
    where paths' = Queue.insertAll paths next
          next = map prep . filter (not.((flip Set.member) seen')) $ neighbors curr
          seen' = Set.insert curr seen
          prep vertex = ((l + 1 + dist vertex), vertex:path)
          l = length path
