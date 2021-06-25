module Computing (
computeCode
) where
import Parsing(Direction)
import Data.Char(intToDigit, toUpper)

computeCode :: (Int -> Direction -> Int) -> [[Direction]] -> Maybe String
computeCode f = Just . map (toUpper.intToDigit) . tail . scanl (foldl f) 5
