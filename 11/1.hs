module One where
import AoC (adventOfCode)
import Parsing (parseInput)
import Computing (calculateStepsToSolve)

main = adventOfCode parseInput calculateStepsToSolve "11" [("1", 11)]
