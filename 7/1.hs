module One where
import AoC (adventOfCode)
import ParsingUtils (lineByLine)
import Data.List(isPrefixOf, partition)
import Seqs(count)
import Parsing(isHypernet, IPv7, parseIPv7, chars)

examples = [("1", 1), ("2", 0), ("3", 0), ("4", 1), ("5", 2)]

containsABBA :: Eq a => [a] -> Bool
containsABBA s
    | length s < 4 = False
    | reverse firstTwo `isPrefixOf` rest && first /= second = True
    | otherwise = containsABBA $ tail s
    where (firstTwo@[first, second], rest) = splitAt 2 s

supportsTLS :: IPv7 -> Bool
supportsTLS addr = any (containsABBA.chars) hyponets && all (not.containsABBA.chars) hypernets
    where (hypernets, hyponets) = partition isHypernet addr

main = adventOfCode (lineByLine parseIPv7) (Just . count supportsTLS) "7" examples
