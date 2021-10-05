module Computing (
    fillAndChecksum
) where 

import Data.List(reverse, unfoldr)
import Seqs(chunksOf)

inv :: String -> String
inv [] = []
inv (x:xs) = (if x == '0' then '1' else '0'):(inv xs)

dragon seed = concat $ seed : unfoldr (Just . next) seed
    where next a = let b = '0' : (inv . reverse $ a) in (b, a ++ b)

checksum :: String -> String
checksum "" = ""
checksum str
    | l `mod` 2 == 1 = str
    | otherwise = checksum str'
    where l = length str
          str' = map reducer $ chunksOf 2 str
          reducer [x, y] = if x == y then '1' else '0'

fillAndChecksum :: Int -> String -> Maybe String
fillAndChecksum 0 _ = Nothing
fillAndChecksum _ "" = Nothing
fillAndChecksum disk seed = Just . checksum . take disk $ dragon seed
