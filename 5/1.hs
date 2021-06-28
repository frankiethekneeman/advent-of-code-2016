module One where
import AoC (adventOfCode, noOp)
import ParsingUtils (oneLine)
import Computing(getInterestingHashes)

examples = [("1", "18f47a30")]

calcPw :: String -> String
calcPw = take 8 . map head . getInterestingHashes 

main = adventOfCode oneLine (Just . calcPw) "5" examples
