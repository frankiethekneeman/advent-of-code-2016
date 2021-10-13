module Computing (
    neighbors,
    dist,
    loc,
) where

import Hashing(md5String)

data Dir = R | L | Up | Down

toChar :: Dir -> Char
toChar R = 'R'
toChar L = 'L'
toChar Down = 'D'
toChar Up = 'U'

isOpen :: String -> Dir -> Bool
isOpen hash dir = elem key "bcdef"
    where key = hash !! idx
          idx = case dir of
            Up -> 0
            Down -> 1
            L -> 2
            R -> 3

possibleDirections :: Int -> Int -> [Dir]
possibleDirections 4 4 = []
possibleDirections x y = lr x ++ ud y
    where lr 1 = [R]
          lr 4 = [L]
          lr _ = [L, R]
          ud 1 = [Down]
          ud 4 = [Up]
          ud _ = [Up, Down]

neighbors :: String -> String -> [String]
neighbors passcode path = map ((path ++).(:"").toChar) $ filter (isOpen hash) $ possibleDirections x y
    where hash = md5String $ passcode ++ path
          (x, y) = loc path

dist :: String -> Int
dist s = 8 - x - y
    where (x, y) = loc s

loc :: String -> (Int, Int)
loc s = (1 + rights - lefts, 1 + downs - ups)
    where rights = length $ filter (=='R') s
          lefts = length $ filter (=='L') s
          downs = length $ filter (=='D') s
          ups = length $ filter (=='U') s



