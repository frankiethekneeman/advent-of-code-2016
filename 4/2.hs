module Two where
import AoC (adventOfCode, noOp)
import Parsing (Room(..), parseRooms)
import Computing (isValidRoom)
import Data.List(elemIndex, find)
import Control.Monad ((<=<))

validLetters :: String
validLetters = ['a'..'z']

letterSpace :: Int
letterSpace = length validLetters

rot :: Int -> Char -> Maybe Char
rot n c = fmap (validLetters !!) idx'
    where idx' = fmap ((`mod` letterSpace) . (+ n)) idx :: Maybe Int
          idx = elemIndex c validLetters :: Maybe Int

rotStr :: Int -> String -> Maybe String
rotStr n = mapM (rot n)

decrypt :: Room -> Maybe Room
decrypt (Room name sectorId checksum) = fmap (\x -> Room x sectorId checksum) name'
    where name' = mapM (rotStr sectorId) name

decryptAll :: [Room] -> Maybe [Room]
decryptAll = mapM decrypt

npoRoomName = ["northpole", "object", "storage"]

findNorthpoleObjectStorage :: [Room] -> Maybe Int
findNorthpoleObjectStorage = fmap sectorId . find ((== npoRoomName) . name) <=< decryptAll

main = adventOfCode parseRooms findNorthpoleObjectStorage "4" []
