module ParsingUtils (
lineByLine,
charByChar,
oneLine
) where

-- Parse a "file" of data one line at a time
lineByLine :: (String -> Maybe a) -> String -> Maybe [a]
lineByLine f = sequence . (map f) . lines 

-- Parse a "line" of data one character at a time
charByChar :: (Char -> Maybe a) -> String -> Maybe [a]
charByChar f = sequence . (map f)

oneLine :: String -> Maybe String
oneLine = Just . init
