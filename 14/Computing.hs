module Computing (
    sixtyFourthKey,
) where

import Numeric(showHex)
import Data.List(tails, replicate, isInfixOf)
import Seqs(collapseRuns, safeHead)
import MonadUtils(orElse)
import Hashing(md5String)

sixtyFourthKey :: (String -> String) -> String -> Int
sixtyFourthKey f salt =  fst $ keys !! 63
    where keys = filter (isKey . snd) indexedHashesWithTails
          indexedHashesWithTails = zip [0..] . map (take 1001) . tails $ hashes
          hashes = map (f . genHash salt) [0..]

isKey :: [String] -> Bool
isKey [] = False
isKey (candidate:rest) = any (`orElse` False) validated
    where validated = map ((validator <*>) . Just) rest
          validator = isInfixOf . replicate 5 <$> trip
          trip = safeHead . map fst . filter ((>= 3) . snd) . collapseRuns $ candidate

genHash :: String -> Int -> String
genHash salt i = md5String salted
    where salted = salt ++ show i
