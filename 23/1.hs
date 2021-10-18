module One where
import AoC (adventOfConfigurableCode)
import Assembunny(readProgram)
import Computing(simulateKeypad)

main = adventOfConfigurableCode readProgram simulateKeypad 7 "23" [("1", 1, 3)]
