module Two where
import AoC (adventOfConfigurableCode)
import Parsing(Dir(..), Instruction(..), parseInstructions)
import Computing (Transform, positionSwap, letterSwap, rotateIns, rotateLetter, rev, move)
import MonadUtils(flatFold, orElseTry)
import Control.Monad(mfilter)

class Undo a where
    undo :: a -> Transform

opposite :: Dir -> Dir
opposite R = L
opposite L = R

instance Undo Instruction where
    undo (PositionSwap f s) = positionSwap (fromIntegral f) (fromIntegral s)
    undo (LetterSwap l r) = letterSwap l r
    undo (Rotation d i) = rotateIns (opposite d) (fromIntegral i)
    undo (LetterRotation c) = undoRotateLetter c
    undo (Reversal f s) = rev (fromIntegral f) (fromIntegral s)
    undo (Move f t) = move (fromIntegral t) (fromIntegral f)

undoRotateLetter :: Char  -> Transform
undoRotateLetter c s = seekUndoneRotateLetter c s s


seekUndoneRotateLetter :: Char -> String -> Transform
seekUndoneRotateLetter c target curr =  test `orElseTry` recurse
    where test = (\_ -> curr) <$> mfilter (==target) (rotateLetter c curr)
          recurse = seekUndoneRotateLetter c target =<< rotateIns L 1 curr

unscramble :: String -> [Instruction] -> Maybe String
unscramble s is = flatFold algorithm s
    where algorithm = reverse $ map undo is

tests = [("1", "decab", "abcde"),
         ("2", "ebcda", "abcde"),
         ("3", "edcba", "abcde"),
         ("4", "abcde", "abcde"),
         ("5", "bcdea", "abcde"),
         ("6", "bdeac", "abcde"),
         ("7", "abdec", "abcde"),
         ("8", "ecabd", "abcde"),
         ("9", "hcdefbag", "abcdefgh")]

main = adventOfConfigurableCode (parseInstructions) unscramble "fbgdceah" "21" tests
