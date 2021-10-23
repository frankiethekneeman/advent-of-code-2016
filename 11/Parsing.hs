module Parsing (
    Item(..),
    State(..),
    parseInput,
) where

import Scanning((^|), (^&), (^*), remember, indicatedByEmptyList, end, get, Parseable(..), grok)
import ParsingUtils(lineByLine)
import qualified Data.Set as Set

data Item = Microchip String | Generator String deriving (Show, Eq, Ord)
data State = State Int [Set.Set Item] deriving Show

instance Parseable Item where
    fromResult r = case get 1 r of
        Just "microchip" -> Microchip <$> get 0 r
        Just "generator" -> Generator <$> get 0 r
        _ -> Nothing

instance Parseable (Set.Set Item) where
    fromResult r = Set.fromList <$> get 0 r

floorNo =  "first" ^| "second" ^| "third" ^| "fourth"
microchip = "-compatible " ^& remember "microchip"
generator = " " ^& remember "generator"
device = "a " ^& (\c -> c /= ' ' && c /= '-') ^& (microchip ^| generator)
listDelimiter = ", and " ^| ", " ^| " and "
emptyList = "nothing relevant" ^& indicatedByEmptyList
items = (device ^* listDelimiter) ^| emptyList
lineScanner = "The " ^& floorNo ^& " floor contains " ^& items ^& "." ^& end

parseInput :: String -> Maybe State
parseInput s = State 0 <$> floors
    where floors = lineByLine (grok lineScanner) s
