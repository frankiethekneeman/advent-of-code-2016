module Two where
import AoC (adventOfConfigurableCode)
import Parsing(parseRanges, Range(..))

combine :: Range -> Range -> Maybe Range
combine l@(Range min _) r@(Range _ max)
    | l > r  = combine r l
    | disjoint l r = Nothing
    | subsumed l r = Just l
    | otherwise = Just $ Range min max

disjoint :: Range -> Range -> Bool
disjoint l@(Range _ maxL) r@(Range minR _)
    | l > r = disjoint r l
    | otherwise = maxL < minR - 1

subsumed :: Range -> Range -> Bool
subsumed (Range minL maxL) (Range minR maxR) = minL <= minR && maxL >= maxR

reduce :: [Range] -> [Range]
reduce [] = []
reduce [x] = [x]
reduce (fst:snd:rest) = case combine fst snd of
    Just combo -> reduce (combo:rest)
    Nothing -> fst:reduce (snd:rest)

size :: Range -> Integer
size (Range min max) = max - min + 1

countAllowed :: Integer -> [Range] -> Maybe Integer
countAllowed field = Just . (field -) . sum . map size . reduce

main = adventOfConfigurableCode parseRanges countAllowed 4294967296 "20" [("1", 10, 2)]
