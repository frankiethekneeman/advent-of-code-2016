module One where
import AoC (adventOfCode)
import Parsing(readDisks)
import Computing(firstPerfectDrop)

main = adventOfCode readDisks firstPerfectDrop "15" [("1", 5)]
