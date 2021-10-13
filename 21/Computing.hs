module Computing (
    Transform,
    positionSwap,
    letterSwap,
    rotateIns,
    rotateLetter,
    rev,
    move,
) where

import Parsing(Dir(..), Instruction(..))
import Data.List(splitAt, elemIndex, uncons)
import Seqs(safeHead, safeTail, rotate)

type Transform = String -> Maybe String
positionSwap :: Int -> Int -> Transform
positionSwap first second s
    | first == second = Just s
    | first > second = positionSwap second first s
    | otherwise = concat <$> sequence [Just begin, y, mid, x, last]
        where (begin, notBegin) = splitAt first s
              y = sequence . (:[]) <$> safeHead =<< (snd <$> postXSplit)
              mid = fst <$> postXSplit
              x = sequence [toX]
              toX = safeHead notBegin :: Maybe Char
              last = safeTail =<< snd <$> postXSplit
              postX = safeTail notBegin
              postXSplit = splitAt  (second - first - 1) <$> postX

letterSwap :: Char -> Char -> Transform
letterSwap l r = Just . map (swap l r)

swap :: Char -> Char -> Char -> Char
swap l r c
    | c == l = r
    | c == r = l
    | otherwise = c

rotateIns :: Dir -> Int -> Transform
rotateIns R c s = rotateIns L (length s - c) s
rotateIns L c s = Just $ rotate rotations s
    where rotations = c `mod` length s

rotateLetter :: Char -> Transform
rotateLetter c s = doRot =<< elemIndex c s
    where doRot i = rotateIns R (i + 1 + if i >=4 then 1 else 0) s

rev :: Int -> Int -> Transform
rev first second str
    | first > second = rev second first str
    | otherwise = Just $ begin ++ (reverse mid) ++ end
    where (begin, rest) = splitAt first str
          (mid, end) = splitAt (second - first + 1) rest

move :: Int -> Int -> Transform
move from to s = case extract from s of
    Just (c, rem) -> let (begin, end) = splitAt to rem
                    in Just $ begin ++ (c:end)
    Nothing -> Nothing

extract :: Int -> String -> Maybe (Char, String)
extract n s = buildResp <$> uncons rest
    where (begin, rest) = splitAt n s
          buildResp (c, end) = (c, begin ++ end)
