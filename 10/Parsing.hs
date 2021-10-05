module Parsing (
parseRuleset,
Rules(..),
Input(..),
BotRules,
Target(..),
) where
import qualified Data.Map.Strict as Map

import ParsingUtils(lineByLine)
import Data.List (stripPrefix, isPrefixOf)
import Seqs(splitOn, forceLength)
import Text.Read (readMaybe)

data Target = Output Int | Bot Int deriving Show
data Instruction = BotRule Int Target Target | Init Target Int
bot_prefix = "bot "
bot_low_infix = " gives low to "
bot_high_infix = " and high to "

output_prefix = "output "
init_prefix = "value "
init_infix = " goes to "

type BotRules = Map.Map Int (Target, Target)
data Input = Input Target Int
data Rules = Rules [Input] BotRules

readInstruction :: String -> Maybe Instruction
readInstruction s
    | init_prefix `isPrefixOf` s =
        let rest = stripPrefix init_prefix s
            valAndBot = forceLength 2 . splitOn init_infix =<< rest
            val = readMaybe . (!!0) =<< valAndBot
            target = readTarget . (!!1) =<< valAndBot
        in Init <$> target <*> val
    | bot_prefix `isPrefixOf` s =
        let infices = stripPrefix bot_prefix s
            botAndTargets = forceLength 2 . splitOn bot_low_infix =<< infices
            bot = readMaybe . (!!0) =<< botAndTargets
            lowAndHigh = forceLength 2 . splitOn bot_high_infix . (!!1) =<< botAndTargets
            low = readTarget . (!!0) =<< lowAndHigh
            high = readTarget . (!!1) =<< lowAndHigh
        in BotRule <$> bot <*> low <*> high
    | otherwise = Nothing

readTarget :: String -> Maybe Target
readTarget s
    | bot_prefix `isPrefixOf` s =  Bot <$> readFix bot_prefix
    | output_prefix `isPrefixOf` s = Output <$> readFix output_prefix
    where readFix p = readMaybe =<< stripPrefix p s

updateRules :: Rules -> Instruction -> Rules
updateRules (Rules inputs bots) (BotRule num low high) = Rules inputs bots'
    where bots' = Map.insert num (low, high) bots
updateRules (Rules inputs bots) (Init bot val) = Rules inputs' bots
    where inputs' = (Input bot val):inputs

buildRuleset :: [Instruction] -> Rules
buildRuleset = foldl updateRules (Rules [] Map.empty)

parseRuleset :: String -> Maybe Rules
parseRuleset s = buildRuleset <$> lineByLine readInstruction s
