module Seqs ( splitOn ) where

import Data.List (isPrefixOf)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list = first:rest
    where (first, remaining) = splitOnce delimiter list
          rest = splitOn delimiter remaining

-- NOT EXPORTED

splitOnce :: Eq a => [a] -> [a] -> ([a], [a])
splitOnce _ [] = ([], [])
splitOnce needle haystack 
  | isPrefixOf needle haystack = ([], prefixDropped)
  | otherwise = (h:l, r)
  where prefixDropped = drop prefixLength haystack
        prefixLength = length needle
        (h:aystack) = haystack
        (l, r) = splitOnce needle aystack
