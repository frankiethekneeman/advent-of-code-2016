module Parsing (
    Triangle(..),
    fromList,
) where

import Data.List(sort)

data Triangle = Triangle { short, middle, long :: Int }

fromList :: [Int] -> Maybe Triangle
fromList ints
    | length ints == 3 = Just $ Triangle short middle long
    | otherwise = Nothing
    where [short, middle, long] = sort ints
