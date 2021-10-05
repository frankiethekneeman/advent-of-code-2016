module Parsing(
    Disk(..),
    readDisks,
) where
import Scanning((^&), grok, scanInt, end, Parseable(..), parse3)
import ParsingUtils(lineByLine)

data Disk = Disk { num, positions, offset :: Integer } deriving (Show)
instance Parseable Disk where fromResult = parse3 Disk

scanDisk = "Disc #" ^& scanInt ^& " has " ^& scanInt ^& " positions; at time=0, it is at position " ^& scanInt ^& "." ^& end

readDisks :: String -> Maybe [Disk]
readDisks = lineByLine $ grok scanDisk
