module MinPQueue (
    MinPQueue,
    empty,
    fromList,
    insert,
    insertAll,
    pop,
    singleton,
) where

import Seqs(chunksOf)

-- Based on a Height Biased Leftist Tree
data MinPQueue p d = Node Int p d (MinPQueue p d) (MinPQueue p d) | Empty

empty = Empty

singleton :: p -> d -> MinPQueue p d
singleton priority datum = Node 1 priority datum Empty Empty

fromList :: Ord p => [(p, d)] -> MinPQueue p d
fromList = mergeAll . map (uncurry singleton)

insert :: Ord p => MinPQueue p d -> p -> d -> MinPQueue p d
insert queue priority datum = merge queue $ singleton priority datum

insertAll :: Ord p => MinPQueue p d -> [(p, d)] -> MinPQueue p d
insertAll queue = merge queue . fromList

pop :: Ord p => MinPQueue p d -> Maybe (d, MinPQueue p d)
pop Empty = Nothing
pop (Node _ _ datum left right) = Just (datum, merge left right)

-- not Exported

rank :: MinPQueue p d -> Int
rank Empty = 0
rank (Node r _ _ _ _) = r

merge :: Ord p => MinPQueue p d -> MinPQueue p d -> MinPQueue p d
merge Empty x = x
merge x Empty = x
merge x@(Node _ px datum left right) y@(Node _ py _ _ _)
    | px > py = merge y x
    | otherwise = Node r px datum left' right'
    where r = 1 + rank right'
          (left', right') = if rank merged > rank left then (merged, left) else (left, merged)
          merged = merge right y

mergeAll :: Ord p => [MinPQueue p d] -> MinPQueue p d
mergeAll [] = Empty
mergeAll [x] = x
mergeAll [x, y] = merge x y
mergeAll xs = mergeAll $ map mergeAll $ chunksOf 2 xs
