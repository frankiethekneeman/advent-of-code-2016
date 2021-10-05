module Two where
import AoC (adventOfConfigurableCode)
import Parsing(parseInput)
import Computing(neighbors)
import qualified Data.Set as Set

solve :: Int -> Int -> Maybe Int
solve steps faveNo = Just . Set.size $ reachable
    where reachable = reachableIn faveNo steps (Set.singleton (1, 1))

reachableIn :: Int -> Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
reachableIn _ 0 seen = seen
reachableIn faveNo n seen = mconcat $ seen':[ Set.fromList . neighbors faveNo $ p | p <- Set.toList seen']
    where seen' = reachableIn faveNo (n - 1) seen

testCases = [("1", 0, 1),
    ("1", 1, 3),
    ("1", 2, 5),
    ("1", 3, 6),
    ("1", 4, 9)]

main = adventOfConfigurableCode parseInput solve 50 "13" testCases
