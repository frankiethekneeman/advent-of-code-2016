module Computing (
    calculateStepsToSolve
) where

import Parsing(Item(..), State(..), parseInput)
import Seqs(update, mapWithIndex)
import AStar
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.List (zipWith)

calculateStepsToSolve :: State -> Maybe Int
calculateStepsToSolve a = (subtract 1) <$> length <$> path 
    where path = AStar.search distance listMoves a

-- Not exported

data OrdableState = OrdableState Int [Int] [Set.Set Item] deriving (Eq, Ord)

instance Eq State where
    l == r = toOrdable l == toOrdable r

instance Ord State where
    compare l r = compare (toOrdable l) (toOrdable r)

toOrdable :: State -> OrdableState
toOrdable (State curr floors) = OrdableState curr countPairs orphans
    where countPairs = map Set.size pairs
          orphans = zipWith elimPaired pairs floors
          pairs = zipWith Set.intersection generatorElements chipElements
          elimPaired names items = items Set.\\ (Set.mapMonotonic Microchip names) Set.\\ (Set.mapMonotonic Generator names)
          generatorElements = map (Set.mapMonotonic element) generators
          chipElements = map (Set.mapMonotonic element) chips
          (generators, chips) = unzip $ map (Set.partition isGenerator) floors

element :: Item -> String
element (Microchip s) = s
element (Generator s) = s

isValidState :: State -> Bool
isValidState (State _ items) = overloaded == 0
    where overloaded = foldl (+) 0 . map (Set.size) . map getOverloaded $ items

getOverloaded :: Set.Set Item -> Set.Set Item
getOverloaded items = if Set.size generators == 0 then Set.empty else orphanedMicrochips
    where orphanedMicrochips = Set.filter checkOrphan microchips
          checkOrphan (Microchip element) = Set.notMember (Generator element) generators
          (generators, microchips) = Set.partition isGenerator items

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator _ = False

listMoves :: State -> [State]
listMoves s@(State curr floors) = ups ++ downs
    where downs = if null singleDowns then mv (-1) doublePayloads else singleDowns
          singleDowns = mv (-1) singlePayloads
          ups = if null doubleUps then mv 1 singlePayloads else doubleUps
          doubleUps = mv 1 doublePayloads
          doublePayloads = plusOne currFloor =<< singlePayloads
          singlePayloads = map Set.singleton $ Set.toList (floors !! curr)
          currFloor = floors !! curr
          itemsBelow = foldl (+) 0 . map Set.size . take curr $ floors
          mv n = (filter isValidState) . catMaybes . (map (move s n)) :: [Set.Set Item] -> [State]

move :: State -> Int -> Set.Set Item -> Maybe State
move (State curr floors) dir payload
    | next >= length floors = Nothing
    | next < 0 = Nothing
    | otherwise = Just $ State next floors''
    where next = dir + curr
          floors'' = update (Set.union payload) next floors'
          floors' = update (Set.\\ payload) curr floors

plusOne :: Ord a => Set.Set a -> Set.Set a -> [Set.Set a]
plusOne adds base = map ((flip Set.insert) base) $ Set.toList (Set.difference adds base)

distance :: State -> Int
distance (State _ floors) = foldl (+) 0 $ mapWithIndex (\no -> \items -> (m - no) * Set.size items) floors
    where m = length floors - 1
