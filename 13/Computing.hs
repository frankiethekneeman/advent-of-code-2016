module Computing (
    manhattanDist,
    neighbors,
) where

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = xDist + yDist
    where xDist = abs $ x1 - x2
          yDist = abs $ y1 - y2

neighbors :: Int -> (Int, Int) -> [(Int, Int)]
neighbors faveNo = filter (isOpen faveNo) . allNeighbors

isOpen :: Int -> (Int, Int) -> Bool
isOpen favNo (x, y) = countSetBits num `mod` 2 == 0
    where num = x'^2 + 3*x' + 2*x'*y' + y' + y'^2 + favNo'
          x' = toInteger x
          y' = toInteger y
          favNo' = toInteger favNo

countSetBits :: Integer -> Integer
countSetBits 0 = 0 
countSetBits n = bit + countSetBits n'
    where (n', bit) = n `divMod` 2

allNeighbors :: (Int, Int) -> [(Int, Int)]
allNeighbors (0, 0) = [(1, 0), (0, 1)]
allNeighbors (x, 0) = [(x + 1, 0), (x - 1, 0), (x, 1)]
allNeighbors (0, y) = [(0, y + 1), (0, y - 1), (0, y)]
allNeighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
