module Assembunny (
    Computer(..),
    Arg(..),
    readProgram,
    readFrom,
    writeRegister,
    runProgram,
    increment,
    Instruction
) where
import qualified Data.Map as Map
import ParsingUtils(lineByLine)
import Scanning((^|),(^&),remember, scanChar, scanInt, end, ReadableFromToken(..), Token(..), Parseable(..), get, grok, alternating)
import MonadUtils(orElse, (<.>), orElseTry)
import Seqs(safeHead, repeatUntilNothing)

-- TYPES
data Computer = Computer {
    insPointer :: Int,
    registers :: Map.Map Char Int,
    output :: [Int], --a STACK, most recent output at the front.
    program :: [Instruction]
} deriving (Show)

data Instruction = Copy Arg Arg
                 | Increment Arg
                 | Decrement Arg
                 | ConditionalJump Arg Arg
                 | Toggle Arg
                 | Output Arg
                 deriving (Eq, Ord, Show)

data Arg = Register Char | Value Int deriving (Eq, Ord, Show)

-- Construction
new :: [Instruction] -> Computer
new = Computer 0 Map.empty []

-- Modification 
readFrom :: Arg -> Computer -> Int
readFrom (Value i) _ = i
readFrom (Register r) c = registerValue `orElse` 0
    where registerValue = Map.lookup r $ registers c

writeRegister :: Char -> Int -> Computer -> Computer
writeRegister r v c = c{registers = registers'}
    where registers' = Map.insert r v $ registers c

writeOutput :: Int -> Computer -> Computer
writeOutput o c = c{output = output'}
    where output' = o:(output c)

jmp :: Int -> Computer -> Computer
jmp dist c = c{insPointer = insPointer'}
    where insPointer' = insPointer c + dist

step :: Computer -> Computer
step = jmp 1

rmw :: Char -> (Int -> Int) -> Computer -> Computer
rmw reg f c = step $ writeRegister reg (f val) c
    where val = readFrom (Register reg) c

toggle :: Instruction -> Instruction
toggle (Increment a) = Decrement a
toggle (ConditionalJump l r) = Copy l r
toggle (Decrement a) = Increment a
toggle (Toggle a) = Increment a
toggle (Output a) = Increment a
toggle (Copy l r) = ConditionalJump l r

-- Command implementations
cpy :: Arg -> Char -> Computer -> Computer
cpy src target c = step $ writeRegister target val c
    where val = readFrom src c

inc :: Char -> Computer -> Computer
inc reg = rmw reg (+1)

dec :: Char -> Computer -> Computer
dec reg = rmw reg (subtract 1)

jnz :: Arg -> Arg -> Computer -> Computer
jnz valSrc distSrc c
    | val == 0 = step c
    | otherwise = jmp dist c
    where val = readFrom valSrc c
          dist = readFrom distSrc c

tgl :: Arg -> Computer -> Computer
tgl distSrc c = step c'
    where c' = if (target < 0) || (target >= length instructions)
            then c
            else c{program = program'}
          program' = before ++ [toggled] ++ after
          instructions = program c
          toggled = toggle $ rest !! 0
          after = drop 1 rest
          (before, rest) = splitAt target instructions
          target = dist + curr
          dist = readFrom distSrc c
          curr = insPointer c

out :: Arg -> Computer -> Computer
out src c = step $ writeOutput val c
    where val = readFrom src c

--PseudoInstructions born from optimization.
add :: Char -> Char -> Computer -> Computer
add src target c = jmp 3 . writeRegister target sum . writeRegister src 0 $ c
    where sum = srcVal + targetVal
          srcVal = readFrom (Register src) c
          targetVal = readFrom (Register target) c

mult :: Arg -> Char -> Char -> Char -> Computer -> Computer
mult op1 op2 target tmp c = jmp 5 . writeRegister target total . writeRegister tmp 0 . writeRegister op2 0 $ c
    where total = op1Val * op2Val + targetVal
          op1Val = readFrom op1 c
          op2Val = readFrom (Register op2) c
          targetVal = readFrom (Register target) c
 
-- Execution
execute :: Instruction -> Computer -> Computer
execute (Copy src (Register r)) = cpy src r
execute (Copy _ _) = step
execute (Increment (Register r)) = inc r
execute (Increment _) = step
execute (Decrement (Register r)) = dec r
execute (Decrement _) = step
execute (ConditionalJump val dist) = jnz val dist
execute (Toggle dist) = tgl dist
execute (Output arg) = out arg


runProgram :: Computer -> Computer
runProgram = last . repeatUntilNothing increment

increment :: Computer -> Maybe Computer
increment c = getInstruction c <*> Just c

type Optimizer = [Instruction] -> Computer -> Maybe (Computer -> Computer)

getInstruction :: Computer -> Maybe (Computer -> Computer)
getInstruction c@(Computer ptr _ _ program) = foldl orElseTry Nothing optimizations
    where optimizations = map attempt optimizers
          attempt o = o upcoming c
          optimizers = [multIns, addIns, oneIns]
          upcoming = drop ptr program

oneIns :: Optimizer
oneIns is _ = execute <$> safeHead is

addIns :: Optimizer
addIns (Increment target:Decrement src:ConditionalJump indicator dist:rest) c
    | hasCorrectPattern && isNotSelfModifying && jumpsToStart && valid = case (src, target) of
        (Register s, Register t) -> Just $ add s t
        _ -> Nothing
    | otherwise = Nothing
    where hasCorrectPattern = target /= src && src == indicator
          isNotSelfModifying = target /= dist && src /= dist
          jumpsToStart = readFrom dist c == -2
          valid = readFrom src c > 0
addIns _ _ = Nothing

multIns :: Optimizer
multIns (Copy src tmp:Increment target:Decrement dec1:ConditionalJump ind1 dist1:Decrement dec2:ConditionalJump ind2 dist2:rest) c
    | hasCorrectPattern && isNotSelfModifying && correctJumps && valid = case (dec2, target, tmp) of
        (Register op2, Register target, Register tmp) -> Just $ mult src op2 target tmp
        _ -> Nothing
    | otherwise = Nothing
    where hasCorrectPattern = dec1 == tmp && dec1 == ind1 && dec2 == ind2 && dec1 /= dec2
          isNotSelfModifying = srcNotModified && distsNotModified
          srcNotModified = not $ src `elem` modified
          distsNotModified = (not (dist1 `elem` modified)) && (not (dist2 `elem` modified))
          modified = [target, dec1, dec2]
          correctJumps = readFrom dist1 c == -2 && readFrom dist2 c == -5
          valid = readFrom src c > 0 && readFrom dec2 c > 0

multIns _ _ = Nothing

-- Parsing
scanArg = (scanInt ^| scanChar)
scanDouble = alternating $ map remember ["cpy", "jnz"]
scanSingle = alternating $ map remember ["inc", "dec", "tgl", "out"]
scanInstruction = ((scanDouble ^& " " ^& scanArg ^& " " ^& scanArg)
                  ^| (scanSingle ^& " " ^& scanArg)) ^& end

instance ReadableFromToken Arg where
    readTok (CharTok s) = Just $ Register s
    readTok (IntTok i) = Just . Value $ fromIntegral i
    readTok _ = Nothing

instance Parseable Instruction where
    fromResult r = case (get 0 r) of
        Just "cpy" -> Copy <$> (get 1 r) <*> (get 2 r)
        Just "inc" -> Increment <$> (get 1 r)
        Just "dec" -> Decrement <$> (get 1 r)
        Just "jnz" -> ConditionalJump <$> (get 1 r) <*> (get 2 r)
        Just "tgl" -> Toggle <$> (get 1 r)
        Just "out" -> Output <$> (get 1 r)
        _ -> Nothing

readProgram :: String -> Maybe Computer
readProgram = new <.> lineByLine (grok scanInstruction)
