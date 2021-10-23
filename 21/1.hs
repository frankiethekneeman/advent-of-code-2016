module One where
import AoC (adventOfConfigurableCode)
import Parsing(Instruction(..), parseInstructions)
import Computing (Transform, positionSwap, letterSwap, rotateIns, rotateLetter, rev, move)
import MonadUtils(flatFold)

class Forward a where
    fwd :: a -> Transform

instance Forward Instruction where
    fwd (PositionSwap f s) = positionSwap (fromIntegral f) (fromIntegral s)
    fwd (LetterSwap l r) = letterSwap l r
    fwd (Rotation d i) = rotateIns d (fromIntegral i)
    fwd (LetterRotation c) = rotateLetter c
    fwd (Reversal f s) = rev (fromIntegral f) (fromIntegral s)
    fwd (Move f t) = move (fromIntegral f) (fromIntegral t)

tests = [("1", "abcde", "decab"),
         ("2", "abcde", "ebcda"),
         ("3", "abcde", "edcba"),
         ("4", "abcde", "abcde"),
         ("5", "abcde", "bcdea"),
         ("6", "abcde", "bdeac"),
         ("7", "abcde", "abdec"),
         ("8", "abcde", "ecabd")]

scramble :: String -> [Instruction] -> Maybe String
scramble s is = flatFold algorithm s
    where algorithm = map fwd is

main = adventOfConfigurableCode parseInstructions scramble "abcdefgh" "21" tests
