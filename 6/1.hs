module One where
import AoC (adventOfCode)
import ParsingUtils (lineByLine)
import Seqs(safeMaximumBy)
import Computing(decodeTransmission, toFrequencies, cmpSnd)

examples = [("1", "easter")]

mode :: Ord a => [a] -> Maybe a
mode = fmap fst . safeMaximumBy cmpSnd . toFrequencies

main = adventOfCode (lineByLine Just) (decodeTransmission mode) "6" examples
