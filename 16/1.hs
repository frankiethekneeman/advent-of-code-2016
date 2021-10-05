module One where
import AoC (adventOfConfigurableCode)
import ParsingUtils(oneLine)
import Computing(fillAndChecksum)

main = adventOfConfigurableCode oneLine fillAndChecksum 272 "16" [("1", 12, "100"), ("2", 20, "01100")]
