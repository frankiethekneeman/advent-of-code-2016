module Two where
import AoC (adventOfCode)
import Parsing(Maze(..), parseMaze)
import Computing(buildAdjacencyList, State(..), countRemaining, next)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified AStar
import Debug.Trace(trace)

touchEveryPointAndReturnHome :: Maze -> Maybe Int
touchEveryPointAndReturnHome maze = fst <$> shortestPath
    where shortestPath = AStar.weighted (distHome places) (next adj) start
          places = Map.keysSet . locations $ maze
          adj = buildAdjacencyList maze
          start = State 0 $ Set.singleton 0
    
distHome :: Set.Set Int -> State -> Int
distHome toSee (State curr seen) = if curr == 0 && remaining == 0
      then 0
      else remaining + 1
    where remaining = Set.size $ Set.difference toSee seen

main = adventOfCode (Just . parseMaze) touchEveryPointAndReturnHome "24" []
