module Computing (
countValid
) where
import Parsing(Triangle(..))

isValid :: Triangle -> Bool
isValid (Triangle s m l) = s + m > l

countValid :: [Triangle] -> Int
countValid = length . filter isValid
