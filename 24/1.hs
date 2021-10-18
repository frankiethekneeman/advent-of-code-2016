module One where
import AoC (adventOfCode)
import Parsing(Maze(..), parseMaze)
import Computing(buildAdjacencyList, State(..), countRemaining, next)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified AStar
import Debug.Trace(trace)

touchEveryPoint :: Maze -> Maybe Int
touchEveryPoint maze = fst <$> shortestPath
    where shortestPath = AStar.weighted (countRemaining places) (next adj) start
          places = Map.keysSet . locations $ maze
          adj = buildAdjacencyList maze
          start = State 0 $ Set.singleton 0

main = adventOfCode (Just . parseMaze) touchEveryPoint "24" [("1", 14)]
