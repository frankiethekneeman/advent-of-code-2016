module Parsing(
    parseRanges,
    Range(..)
)where
import ParsingUtils(lineByLine)
import Data.List(sort)
import Scanning((^&), scanInt, Parseable(..), parse2, grok)

data Range = Range Integer Integer deriving (Eq, Ord, Show)
instance Parseable Range where fromResult = parse2 Range

rangeScanner = scanInt ^& "-" ^& scanInt

parseRanges :: String -> Maybe [Range]
parseRanges = (sort <$>) . (lineByLine $ grok rangeScanner)
