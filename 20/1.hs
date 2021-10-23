module One where
import AoC (adventOfCode)
import Parsing(parseRanges, Range(..))

smallestUnblocked :: Integer -> [Range] -> Integer
smallestUnblocked i [] = i
smallestUnblocked i (Range l h:rest)
    | i < l  = i
    | i < h = smallestUnblocked (h + 1) rest
    | otherwise = smallestUnblocked i rest 

main = adventOfCode parseRanges (Just . smallestUnblocked 0) "20" [("1", 3)]
