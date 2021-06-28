module Two where
import AoC (adventOfCode)
import ParsingUtils (lineByLine)
import Seqs(safeMinimumBy)
import Computing(decodeTransmission, toFrequencies, cmpSnd)

examples = [("1", "advent")]

antiMode :: Ord a => [a] -> Maybe a
antiMode = fmap fst . safeMinimumBy cmpSnd . toFrequencies

main = adventOfCode (lineByLine Just) (decodeTransmission antiMode) "6" []
