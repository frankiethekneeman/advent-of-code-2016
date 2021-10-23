module Two where
import AoC (adventOfCode)
import Text.Read(readMaybe)

whichElfGetsPresents :: Int -> Int
whichElfGetsPresents start
    | r == 0 = start
    | r < pow3 = r
    | otherwise = pow3 + 2 * (r - pow3)
    where r = start - pow3
          pow3 = biggestPowerOf3 start

biggestPowerOf3 :: Int -> Int
biggestPowerOf3 x
    | x < 3 = 1
    | otherwise = 3 * biggestPowerOf3 (x `div` 3)

main = adventOfCode (readMaybe :: String -> Maybe Int) (Just . whichElfGetsPresents) "19" [("1", 2)]
