module Parsing (
    Dir(..),
    Instruction(..),
    parseInstructions
) where

import ParsingUtils(lineByLine)
import Scanning((^&), (^|), scanInt, end, grok, indicatedBy, remember, Parseable(..), get, scanChar)

posSwapScanner = "swap position " ^& scanInt ^& " with position " ^& scanInt ^& end
letterSwapScanner = "swap letter " ^& scanChar ^& " with letter " ^& scanChar ^& end
rotateNScanner = "rotate " ^& (remember "left" ^| remember "right") ^& " " ^& scanInt ^& (" steps" ^| " step") ^& end
rotateLetterScanner = "rotate based on position of letter " ^& scanChar ^& end
reverseScanner = "reverse positions " ^& scanInt ^& " through " ^& scanInt ^& end
moveScanner = "move position " ^& scanInt ^& " to position " ^& scanInt ^& end

instructionScanner = foldl1 (^|) $ map (\(x, y) -> indicatedBy x ^& y) [("positionSwap", posSwapScanner),
    ("letterSwap", letterSwapScanner),
    ("rotateN", rotateNScanner),
    ("rotateLetter", rotateLetterScanner),
    ("reverse", reverseScanner),
    ("move", moveScanner)]


data Dir = L | R

fromString :: String -> Dir
fromString "left" = L
fromString "right" = R

data Instruction = PositionSwap Integer Integer
                | LetterSwap Char Char
                | Rotation Dir Integer
                | LetterRotation Char
                | Reversal Integer Integer
                | Move Integer Integer

instance Parseable Instruction where
    fromResult r = case get 0 r of 
        Just "positionSwap" -> PositionSwap <$> get 1 r <*> get 2 r
        Just "letterSwap" -> LetterSwap <$> get 1 r <*> get 2 r
        Just "rotateN" -> Rotation <$> (fromString <$> get 1 r) <*> get 2 r
        Just "rotateLetter" -> LetterRotation <$> get 1 r
        Just "reverse" -> Reversal <$> get 1 r <*> get 2 r
        Just "move" -> Move <$> get 1 r <*> get 2 r
        Nothing -> Nothing

parseInstructions :: String -> Maybe [Instruction]
parseInstructions = lineByLine $ grok instructionScanner
