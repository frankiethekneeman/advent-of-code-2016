module One where
import AoC (adventOfConfigurableCode)
import Parsing(parseTiles)
import Computing(testCases, countSafeTiles)

main = adventOfConfigurableCode parseTiles countSafeTiles 40 "18" testCases
