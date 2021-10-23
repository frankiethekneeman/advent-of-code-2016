module Two where
import AoC (adventOfCode, noOp)
import ParsingUtils (oneLine)
import Computing(getInterestingHashes)
import Data.Char (digitToInt)
import Data.List(find)

examples = [("1", "05ace8e3")]

replaceChar :: String -> (Int, Char) -> String
replaceChar str (pos, char) = take pos str ++ [char] ++ drop (pos + 1) str

calcPw :: String -> Maybe String
calcPw id = mapM charAt [0..7]
    where characters = map toTuple $ getInterestingHashes id
          toTuple hash = (digitToInt $ head hash, head $ tail hash)
          charAt p = snd <$> find ((==p).fst) characters

main = adventOfCode oneLine calcPw "5" examples
