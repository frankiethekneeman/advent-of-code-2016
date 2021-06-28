module Computing (
decodeTransmission,
toFrequencies,
cmpSnd
) where

import Data.List(transpose, sort)
import Seqs(collapseRuns)

decodeTransmission :: (String -> Maybe Char) -> [String] -> Maybe String
decodeTransmission pick = sequence . map pick . transpose

toFrequencies :: Ord a => [a] -> [(a, Int)]
toFrequencies = collapseRuns . sort
    
cmpSnd :: Ord b => (a, b) -> (a, b) -> Ordering
cmpSnd (_, l) (_, r) = compare l r
