module Seqs (
    chunksOf,
    collapseRuns,
    count,
    forceLength,
    mapWithIndex,
    repeatUntilNothing,
    rotate,
    safeHead,
    safeInit,
    safeLast,
    safeMaximum,
    safeMaximumBy,
    safeMinimumBy,
    safeTail,
    splitOn,
    update,
) where

import Data.List (isPrefixOf, group, maximumBy, minimumBy, splitAt, zipWith, unfoldr)

rotate :: Int -> [a] -> [a]
rotate n l = back ++ front 
    where (front, back) = splitAt n l

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list = first:rest
    where (first, remaining) = splitOnce delimiter list
          rest = splitOn delimiter remaining

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = first:remaining
    where (first, rest) = splitAt n l
          remaining = chunksOf n rest

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

collapseRuns :: Eq a => [a] -> [(a, Int)]
collapseRuns = map toTuple . group
    where toTuple xs = (head xs, length xs)

safeHead = defang head
safeTail = defang tail
safeInit = defang init
safeLast = defang last
safeMaximumBy = defang2 maximumBy
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum = safeMaximumBy compare
safeMinimumBy = defang2 minimumBy

update :: (a -> a) -> Int -> [a] -> [a]
update f i xs
    | length xs > i = before ++ [updated] ++ after
    | otherwise = xs
    where updated = f toUpdate
          after = tail rest
          toUpdate = head rest
          (before, rest) = splitAt i xs

forceLength :: Int -> [a] -> Maybe [a]
forceLength n l
    | length l == n = Just l
    | otherwise = Nothing

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys)
    | x <= y = x:mergeSorted xs (y:ys)
    | otherwise = y:mergeSorted (x:xs) ys

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

repeatUntilNothing :: (a -> Maybe a) -> a -> [a]
repeatUntilNothing f = unfoldr (calcNext<$>) . Just
    where calcNext x = (x, f x)

-- NOT EXPORTED

splitOnce :: Eq a => [a] -> [a] -> ([a], [a])
splitOnce _ [] = ([], [])
splitOnce needle haystack 
  | needle `isPrefixOf` haystack = ([], prefixDropped)
  | otherwise = (h:l, r)
  where prefixDropped = drop prefixLength haystack
        prefixLength = length needle
        (h:aystack) = haystack
        (l, r) = splitOnce needle aystack

defang :: ([a] -> b) -> [a] -> Maybe b
defang _ [] = Nothing
defang f l = Just $ f l

defang2 :: (a -> [b] -> c) -> a -> [b] -> Maybe c
defang2 _ _ [] = Nothing
defang2 f a l = Just $ f a l
