module Two where
import AoC (adventOfCode, noOp)
import ParsingUtils (oneLine)
import Computing(Marker(..),decompressionParts)

examples = [("1", 6), ("2", 7), ("3", 9), ("4", 11), ("5", 3), ("6", 20),
            ("7", 241920), ("8", 445)]

--decompressedLength :: String -> Int
--decompressedLength s = case decompressionParts s of
--                        (prefix, Nothing, _, _) -> length prefix
--                        (prefix, Just (Marker _ reps), toRepeat, toRecurse) -> 
--                            let prefixLen = length prefix
--                                repeatLen = reps * (decompressedLength toRepeat)
--                                recursedLen = decompressedLength toRecurse
--                            in prefixLen + repeatLen + recursedLen
decompress :: String -> String
decompress s = case decompressionParts s of
                        (prefix, Nothing, _, _) -> prefix
                        (prefix, Just (Marker _ reps), toRepeat, toRecurse) -> 
                            let repeats = concat $ replicate reps $ decompress toRepeat
                                recursed = decompress toRecurse
                            in prefix ++ repeats ++ recursed

decompressedLength :: String -> Maybe Int
decompressedLength = Just . length . decompress

main = adventOfCode oneLine (decompressedLength) "9" examples
