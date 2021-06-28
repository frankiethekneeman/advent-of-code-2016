module One where
import AoC (adventOfCode)
import Parsing (Room(..), parseRooms)
import Computing (isValidRoom)

examples = [("1",123), ("2",987), ("3", 404), ("4", 0), ("5", 1514)]

sumSectorIdsOfValidRooms :: [Room] -> Int
sumSectorIdsOfValidRooms = foldl (+) 0 . map sectorId . filter isValidRoom

main = adventOfCode parseRooms (Just . sumSectorIdsOfValidRooms) "4" examples
