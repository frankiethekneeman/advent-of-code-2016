module One where
import AoC (adventOfCode)
import Text.Read(readMaybe)

getWinner :: Int -> Int
getWinner contestants = 2 * l + 1
    where l = contestants - largestSetBit contestants

largestSetBit :: Int -> Int
largestSetBit 1 = 1
largestSetBit n = 2 * largestSetBit (n `div` 2)

main = adventOfCode (readMaybe :: String -> Maybe Int) (Just . getWinner) "19" [("1", 3)]
