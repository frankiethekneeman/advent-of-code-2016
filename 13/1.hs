module One where
import AoC (adventOfConfigurableCode)
import Parsing(parseInput)
import Computing(manhattanDist, neighbors)
import AStar(search)

findPathLength :: (Int, Int) -> Int -> Maybe Int
findPathLength target faveNo = (subtract 1 . length) <$> path
    where path = search dist (neighbors faveNo) (1, 1)
          dist = manhattanDist target
            

main = adventOfConfigurableCode parseInput findPathLength (31,39) "13" [("1", (7,4), 11)]
