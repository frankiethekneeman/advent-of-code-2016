module Parsing (
    parseInput,
    Computer(..),
    readRegister,
    writeRegister,
) where
import Control.Monad((<=<))
import Scanning((^|),(^&),remember, scanChar, scanInt, end, ReadableFromToken(..), Token(..), Parseable(..), get, grok)
import ParsingUtils(lineByLine)
import Data.List(intercalate, zipWith)

type Instruction = Computer -> Computer
data Computer = Computer Int Int Int Int Int [Instruction]

instance Show Computer where
    show (Computer ptr a b c d p) = (intercalate ", " state) ++ " @" ++ (show ptr) ++ "/" ++ (show $ length p)
        where state = zipWith (++) labels registers
              labels = map (++ ": ") ["A", "B", "C", "D"]
              registers = map show [a, b, c, d]

initComp :: [Instruction] -> Computer
initComp = Computer 0 0 0 0 0

readRegister :: Char -> Computer -> Int
readRegister 'a' (Computer _ a _ _ _ _) = a
readRegister 'b' (Computer _ _ b _ _ _) = b
readRegister 'c' (Computer _ _ _ c _ _) = c
readRegister 'd' (Computer _ _ _ _ d _) = d

writeRegister :: Char -> Int -> Instruction
writeRegister 'a' v (Computer ins _ b c d program) = Computer ins v b c d program
writeRegister 'b' v (Computer ins a _ c d program) = Computer ins a v c d program
writeRegister 'c' v (Computer ins a b _ d program) = Computer ins a b v d program
writeRegister 'd' v (Computer ins a b c _ program) = Computer ins a b c v program

jmp :: Int -> Instruction
jmp dist (Computer ins a b c d program) = Computer (ins + dist) a b c d program

step :: Instruction
step  = jmp 1

cpy :: (Computer -> Int) -> Char -> Instruction
cpy reader writeLoc comp = step $ writeRegister writeLoc (reader comp) comp

delta :: Int -> Char -> Instruction
delta d register comp = step $ writeRegister register (curr + d) comp
    where curr = readRegister register comp

dec :: Char -> Instruction
dec = delta (-1)

inc :: Char -> Instruction
inc = delta 1

jnz :: (Computer -> Int) -> Int -> Instruction
jnz reader dist comp = if reader comp == 0 then step comp else jmp dist comp

scanParam = scanInt ^| scanChar

insScanner = ((remember "cpy " ^& scanParam ^& " " ^& scanChar)
            ^| ((remember "inc " ^| remember "dec ") ^& scanChar)
            ^| (remember "jnz " ^& scanParam ^& " " ^& scanInt)) ^& end

instance ReadableFromToken (Computer -> Int) where
    readTok (CharTok s) = Just $ readRegister s
    readTok (IntTok i) = Just (\_ -> fromIntegral i)
    readTok _ = Nothing

instance Parseable Instruction where
    fromResult r = case (get 0 r) of
        Just "cpy " -> cpy <$> (get 1 r) <*> (get 2 r)
        Just "inc " -> inc <$> (get 1 r)
        Just "dec " -> dec <$> (get 1 r)
        Just "jnz " -> jnz <$> (get 1 r) <*> (fromIntegral <$> (get 2 r :: Maybe Integer))
        _ -> Nothing

parseInput :: String -> Maybe Computer
parseInput = init <=< parseInstructions
    where init = Just . initComp
          parseInstructions = lineByLine (grok insScanner)

