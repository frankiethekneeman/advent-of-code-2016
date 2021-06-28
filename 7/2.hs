module Two where
import AoC (adventOfCode, noOp)
import ParsingUtils (lineByLine)
import Data.List(isPrefixOf, partition)
import Parsing(isHypernet, IPv7, parseIPv7, chars)
import Seqs(count)
import qualified Data.Set as Set

examples = [("1", 0), ("2", 0), ("3", 0), ("4", 0), ("5", 0),
            ("6", 1), ("7", 0), ("8", 1), ("9", 1), ("10", 3)]

getABAs :: String -> Set.Set String
getABAs s
    | length s < 3 = Set.empty
    | a1 == a2 && b /= a1 = Set.insert [a1, b, a1] rest
    | otherwise = rest
    where rest = getABAs $ tail s
          a1:b:a2:t = s

toBAB :: String -> Set.Set String
toBAB s
    | length s /= 3 = Set.empty
    | otherwise = Set.singleton [b, a, b]
    where [a, b, _] = s

supportsSSL :: IPv7 -> Bool
supportsSSL addr = not . null $ Set.intersection requiredBABs foundBABs
    where requiredBABs = Set.foldl Set.union Set.empty $ Set.map toBAB foundABAs
          foundBABs = abaSearch hypernets
          foundABAs = abaSearch hyponets
          abaSearch = foldl Set.union Set.empty . map (getABAs.chars)
          (hypernets, hyponets) = partition isHypernet addr

main = adventOfCode (lineByLine parseIPv7) (Just . count supportsSSL) "7" examples
