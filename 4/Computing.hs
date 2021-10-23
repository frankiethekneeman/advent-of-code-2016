module Computing (
    isValidRoom,
) where

import Seqs (collapseRuns)
import Data.List (sort, sortBy)
import Data.Monoid (mappend)
import Parsing (Room(..))

isValidRoom :: Room -> Bool
isValidRoom r = givenChecksum == calculatedChecksum
    where givenChecksum = checksum r
          calculatedChecksum = calcChecksum r

calcChecksum :: Room -> String
calcChecksum = map fst . take 5 . sortBy countThenValue . collapseRuns . sort . concat . name
    where countThenValue (charL, countL) (charR, countR) = compare countR countL `mappend` compare charL charR
