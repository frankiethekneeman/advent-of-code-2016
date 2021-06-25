module AoC (
adventOfCode,
noOp
) where

import Solving (solve, showSolution, Parser, Computor)
import Testing (allTestCases, showTestResult, testsPass, TestCase)

adventOfCode :: Eq b => Show b => Parser a -> Computor a b -> String -> [TestCase b] -> IO ()
adventOfCode parseF computeF day cases = do
    results <- allTestCases parseF computeF day cases
    sequence $ map (putStrLn . showTestResult) results
    putStrLn ""
    if testsPass results
        then do
            answer <- solve parseF computeF (day ++ "/input")
            putStrLn $ showSolution answer 
        else putStrLn "Test failure - please fix before attempting on full problem"
    return ()

noOp :: a -> Maybe b
noOp _ = Nothing
