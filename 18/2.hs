module Two where
import AoC (adventOfConfigurableCode)
import Parsing(parseTiles)
import Computing(testCases, countSafeTiles)

main = adventOfConfigurableCode parseTiles countSafeTiles 400000 "18" testCases
