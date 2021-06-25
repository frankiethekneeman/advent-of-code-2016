module Two where
import AoC (adventOfCode, noOp)

main = adventOfCode (noOp :: String -> Maybe Int) (noOp :: Int -> Maybe Int) "10" []
