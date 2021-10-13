module Two where
import AoC (adventOfCode)
import ParsingUtils (oneLine)
import Computing(sixtyFourthKey)
import Hashing(md5String)
import Debug.Trace(trace)

reApply :: Int -> (a -> a) -> a -> a
reApply 0 _ = id
reApply n f = f . reApply (n - 1) f


stretch :: String -> String
stretch = reApply 2016 md5String

main = adventOfCode oneLine (Just . sixtyFourthKey stretch) "14" [("1", 22551)]
