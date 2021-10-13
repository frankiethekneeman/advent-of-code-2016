module One where
import AoC (adventOfCode)
import ParsingUtils(oneLine)
import Computing(neighbors, dist)
import qualified AStar
tests = [("1", "DDRRRD"),
         ("2", "DDUDRLRRUDRD"),
         ("3", "DRURDRUDDLLDLUURRDULRLDUUDDDRR")]

solve :: String -> Maybe String
solve passcode = head <$> AStar.uniformWeights dist (neighbors passcode) "" 

main = adventOfCode oneLine solve "17" tests
