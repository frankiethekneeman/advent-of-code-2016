module One where
import AoC (adventOfCode, noOp)
import Assembunny(Computer(..), readProgram, increment, Instruction, writeRegister)
import Seqs(safeHead)
import qualified Data.Map as Map
import Data.List(isPrefixOf)
import Seqs(repeatUntilNothing)

data State = State {
    insPointer :: Int,
    registers :: Map.Map Char Int,
    program :: [Instruction]
} deriving (Show, Eq, Ord)

clockSignal :: [Int]
clockSignal = cycle [0,1]

state :: Computer -> State
state (Computer i r _ p) = State i r p

firstIntGeneratesClockSignal :: Computer -> Maybe Int
firstIntGeneratesClockSignal c = fst <$> safeHead clocks
    where clocks = filter (generatesClockSignal . snd) indexedCandidates
          indexedCandidates = zipWith (,) ints candidates
          candidates = map (($ c) . writeRegister 'a') ints
          ints = [1..]

generatesClockSignal :: Computer -> Bool
generatesClockSignal c = not terminates && (generated `isPrefixOf` clockSignal)
    where generated = prefix ++ infinite ++ infinite
          terminates = null infinite
          (prefix, infinite) = determineOutput Map.empty states
          states = repeatUntilNothing increment c

determineOutput :: Map.Map State [Int] -> [Computer] -> ([Int], [Int])
determineOutput seen (c:rest)
    | Map.member s seen = (prefix, infinite)
    | null rest = (reverse out, [])
    | otherwise = determineOutput seen' rest
    where prefix = reverse $ seen Map.! s
          infinite = reverse . take (length out - length prefix) $ out
          seen' = Map.insert s out seen
          out = output c
          s = state c

main = adventOfCode readProgram firstIntGeneratesClockSignal "25" []
