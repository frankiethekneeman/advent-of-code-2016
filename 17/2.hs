module Two where
import AoC (adventOfCode, noOp)
import ParsingUtils(oneLine)
import Computing(neighbors, loc)
import Seqs(safeMaximum)
import MonadUtils(orElseTry)

tests = [("1", 370),
         ("2", 492),
         ("3", 830)]

maxPathLength :: [String] -> String -> Maybe Int
maxPathLength [] _ = Nothing
maxPathLength paths passcode = greaterPath `orElseTry` longestComplete
    where greaterPath = maxPathLength paths' passcode
          paths' = concatMap (neighbors passcode) paths
          longestComplete = safeMaximum lengths
          lengths = map length complete
          complete = filter ((==(4, 4)).loc) paths

main = adventOfCode oneLine (maxPathLength [""]) "17" tests
