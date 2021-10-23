module Computing (
    State(..),
    increment,
    isFrozen,
    openingState,
) where

import Parsing(Rules(..), Input(..), BotRules, Target(..))
import qualified Data.Map.Strict as Map
import Data.List (sort)

type BotStorage = Map.Map Int [Int]
type OutputStorage = Map.Map Int Int
type Update = (Target, Int)
data State = State BotStorage OutputStorage deriving Show

put :: BotStorage -> OutputStorage -> Update -> (BotStorage, OutputStorage)
put bots outputs (Bot no, x) = (bots', outputs)
    where bots' = Map.insert no storage' bots
          storage' = sort $ x:storage
          storage = Map.findWithDefault [] no bots
put bots outputs (Output no, x) = (bots, outputs')
    where outputs' = Map.insert no x outputs

putAll :: BotStorage -> OutputStorage -> [Update] -> (BotStorage, OutputStorage)
putAll b s = foldl (uncurry put) (b, s)

toUpdates :: BotRules -> Int -> [Int] -> [(Target, Int)]
toUpdates rules botNo [low, high] = [(lowTarget, low), (highTarget, high)]
    where (lowTarget, highTarget) = rules Map.! botNo

increment :: Rules -> State -> State
increment (Rules _ botRules) (State bots outputs) = State bots' outputs'
    where (bots', outputs') = putAll inProgressBots outputs updates
          (fullBots, inProgressBots) = Map.partition ((==2) . length) bots
          updates = concatMap (uncurry (toUpdates botRules)) $ Map.toList fullBots

openingState :: Rules -> State
openingState (Rules inputs botRules ) = State bots outputs
    where (bots, outputs) = putAll Map.empty Map.empty updates
          updates = map (\(Input t v) -> (t, v)) inputs

isFrozen :: State -> Bool
isFrozen (State bots _) = (0 ==) $ Map.size $ Map.filter ((==2) . length) bots
