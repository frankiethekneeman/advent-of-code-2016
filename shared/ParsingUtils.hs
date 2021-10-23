module ParsingUtils (
    charByChar,
    lineByLine,
    oneLine
) where

-- Parse a "file" of data one line at a time
lineByLine :: (String -> Maybe a) -> String -> Maybe [a]
lineByLine f = mapM f . lines 

-- Parse a "line" of data one character at a time
charByChar :: (Char -> Maybe a) -> String -> Maybe [a]
charByChar = mapM

oneLine :: String -> Maybe String
oneLine = Just . init
