module Computing (
    testCases,
    countSafeTiles,
) where
import Data.List(unfoldr, zipWith, tails)
import Parsing(Tile(..))

testCases :: [(String, Int, Int)]
testCases = [("1", 3, 6),
         ("2", 10, 38)]

countSafeTiles :: Int -> [Tile] -> Maybe Int
countSafeTiles nRows startRow = Just $ foldr (+) 0 lengths
    where lengths = map length . map (filter (==Safe)) $ room
          room = take nRows $ rows startRow

rows :: [Tile] -> [[Tile]]
rows = unfoldr f
    where f row = Just (row, next row)

next :: [Tile] -> [Tile]
next r = (map calcTile) . zipWith (flip const) r $ windows
    where windows = map (take 3) $ tails (Safe:r ++ [Safe])

calcTile :: [Tile] -> Tile
calcTile [l, _, r]
    | l /= r = Trap
    | otherwise = Safe

