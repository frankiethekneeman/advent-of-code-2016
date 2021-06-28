module Parsing(
IPv7,
isHypernet,
chars,
parseIPv7
) where

import Seqs(safeTail)
import Data.List(span)
import MonadUtils(orElse)

data SequenceType = Hypernet | Hyponet 
data Sequence = Sequence SequenceType String
type IPv7 = [Sequence]

endDelim :: SequenceType -> Char
endDelim Hypernet = ']'
endDelim Hyponet = '['

getRemainder :: SequenceType -> String -> Maybe String
getRemainder Hypernet s = safeTail s
getRemainder Hyponet s = Just $ orElse (safeTail s) ""

next :: SequenceType -> SequenceType
next Hypernet = Hyponet
next Hyponet = Hypernet

chars :: Sequence -> String
chars (Sequence _ s) = s

isHypernet :: Sequence -> Bool
isHypernet (Sequence Hypernet _) = True
isHypernet _ = False


parseIPv7 :: String -> Maybe IPv7
parseIPv7 = pullSequences Hyponet

pullSequences :: SequenceType -> String -> Maybe IPv7
pullSequences _ "" = Just []
pullSequences t str = fmap ((Sequence t lead):) rest'
    where (lead, rest) = span (/=(endDelim t)) str
          rest' = pullSequences (next t) =<< getRemainder t rest
