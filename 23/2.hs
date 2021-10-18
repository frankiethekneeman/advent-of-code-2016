module Two where
import AoC (adventOfConfigurableCode)
import Assembunny(readProgram)
import Computing(simulateKeypad)

main = adventOfConfigurableCode readProgram simulateKeypad 12 "23" [("1", 1, 3)]
