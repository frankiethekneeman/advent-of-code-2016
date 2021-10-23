module Parsing (
    BotRules,
    Input(..),
    Rules(..),
    Target(..),
    parseRuleset,
) where

import qualified Data.Map.Strict as Map
import ParsingUtils(lineByLine)
import Data.List (stripPrefix, isPrefixOf)
import Seqs(splitOn, forceLength)
import Text.Read (readMaybe)

data Target = Output Int | Bot Int deriving Show
data Instruction = BotRule Int Target Target | Init Target Int
botPrefix = "bot "
botLowInfix = " gives low to "
botHighInfix = " and high to "

outputPrefix = "output "
initPrefix = "value "
initInfix = " goes to "

type BotRules = Map.Map Int (Target, Target)
data Input = Input Target Int
data Rules = Rules [Input] BotRules

readInstruction :: String -> Maybe Instruction
readInstruction s
    | initPrefix `isPrefixOf` s =
        let rest = stripPrefix initPrefix s
            valAndBot = forceLength 2 . splitOn initInfix =<< rest
            val = readMaybe . (!!0) =<< valAndBot
            target = readTarget . (!!1) =<< valAndBot
        in Init <$> target <*> val
    | botPrefix `isPrefixOf` s =
        let infices = stripPrefix botPrefix s
            botAndTargets = forceLength 2 . splitOn botLowInfix =<< infices
            bot = readMaybe . (!!0) =<< botAndTargets
            lowAndHigh = forceLength 2 . splitOn botHighInfix . (!!1) =<< botAndTargets
            low = readTarget . (!!0) =<< lowAndHigh
            high = readTarget . (!!1) =<< lowAndHigh
        in BotRule <$> bot <*> low <*> high
    | otherwise = Nothing

readTarget :: String -> Maybe Target
readTarget s
    | botPrefix `isPrefixOf` s =  Bot <$> readFix botPrefix
    | outputPrefix `isPrefixOf` s = Output <$> readFix outputPrefix
    where readFix p = readMaybe =<< stripPrefix p s

updateRules :: Rules -> Instruction -> Rules
updateRules (Rules inputs bots) (BotRule num low high) = Rules inputs bots'
    where bots' = Map.insert num (low, high) bots
updateRules (Rules inputs bots) (Init bot val) = Rules inputs' bots
    where inputs' = Input bot val:inputs

buildRuleset :: [Instruction] -> Rules
buildRuleset = foldl updateRules (Rules [] Map.empty)

parseRuleset :: String -> Maybe Rules
parseRuleset s = buildRuleset <$> lineByLine readInstruction s
