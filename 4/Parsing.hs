module Parsing(
Room(..),
parseRooms
) where
import Seqs (splitOn, safeHead, safeInit, safeLast, collapseRuns)
import Text.Read (readMaybe)
import ParsingUtils(lineByLine)

data Room = Room {
  name :: [String],
  sectorId :: Int,
  checksum :: String
} deriving (Show, Eq)

parseRooms :: String -> Maybe [Room]
parseRooms = lineByLine parseRoom

parseRoom :: String -> Maybe Room
parseRoom input = Room <$> name <*> sectorId <*> checksum
    where name = safeInit segments
          sectorId = readMaybe =<< safeHead =<< sectorAndChecksum :: Maybe Int
          checksum = take 5 <$> (safeLast =<< sectorAndChecksum)
          segments = splitOn "-" input
          sectorAndChecksum = splitOn "[" <$> (safeLast segments) :: Maybe [String]

