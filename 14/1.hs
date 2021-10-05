module One where
import AoC (adventOfCode)
import ParsingUtils (oneLine)
import Computing(sixtyFourthKey)

main = adventOfCode oneLine (Just . sixtyFourthKey id) "14" [("1", 22728)]
