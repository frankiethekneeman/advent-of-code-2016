module Computing (
    runProgram
) where

import Parsing (Computer(..), parseInput, readRegister)

runProgram :: Computer -> Computer
runProgram c@(Computer ptr _ _ _ _ program) = if (ptr < 0) || (ptr >= length program)
        then c
        else runProgram $ instruction c
    where instruction = program !! ptr
