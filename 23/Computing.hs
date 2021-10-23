module Computing (
    simulateKeypad,
) where

import Assembunny(Computer, readFrom, Arg(Register), writeRegister, runProgram)

simulateKeypad :: Int -> Computer -> Maybe Int
simulateKeypad i = Just . readFrom (Register 'a') . runProgram . writeRegister 'a' i
