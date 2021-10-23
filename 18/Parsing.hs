module Parsing(
    Tile(..),
    parseTiles,
) where
import ParsingUtils (oneLine, charByChar)

data Tile = Safe | Trap deriving (Eq, Show)

fromChar :: Char -> Tile
fromChar '.' = Safe
fromChar '^' = Trap

parseTiles :: String -> Maybe [Tile]
parseTiles = (charByChar (Just .fromChar) =<<) . oneLine
