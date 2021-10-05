module Computing (
Marker(..),
decompressionParts,
) where
import Data.List(break, replicate, stripPrefix)
import Seqs(splitOn, forceLength)
import MonadUtils(orElse)
import Text.Read(readMaybe)
pair :: a -> b -> (a, b)
pair a b = (a, b)

pullDecompressedPrefix :: String -> (String, String)
pullDecompressedPrefix = break (=='(')

data Marker = Marker {chars, reps :: Int} deriving (Show)

getAddressed :: (Marker, String) -> (String, String)
getAddressed ((Marker chars _), s) = splitAt chars s

getMarker :: String -> Maybe (Marker, String)
getMarker s = pair <$> marker <*> rest
    where rest = stripPrefix ")" =<< snd <$> pieces
          marker = parseMarker =<< fst <$> pieces
          pieces = break (==')') <$> stripPrefix "(" s

parseMarker :: String -> Maybe Marker
parseMarker s = Marker <$> length <*> repititions
    where length = readMaybe =<< (!!0) <$> lw
          repititions = readMaybe =<< (!!1) <$> lw
          lw = forceLength 2 $ splitOn "x" s

decompressionParts :: String -> (String, Maybe Marker, String, String)
decompressionParts s = (prefix, marker, toRepeat, rest)
    where (prefix, markerWithTail) = pullDecompressedPrefix s
          markerM = getMarker markerWithTail
          marker = fst <$> markerM
          (toRepeat, rest) = orElse (getAddressed <$> markerM) ("", "")

