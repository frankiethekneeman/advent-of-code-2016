module Two where
import AoC (adventOfConfigurableCode)
import ParsingUtils(oneLine)
import Computing(fillAndChecksum)

main = adventOfConfigurableCode oneLine fillAndChecksum 35651584 "16" [("1", 12, "100"), ("2", 20, "01100")]
