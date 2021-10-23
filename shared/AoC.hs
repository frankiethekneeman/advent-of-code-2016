module AoC (
    adventOfCode,
    adventOfConfigurableCode,
    noOp,
    traceNoOp,
) where

import Solving (solve, showSolution, Parser, Computor, Solution, ConfigurableComputor)
import Testing (allTestCases, showTestResult, testsPass, TestCase, ConfigurableTestCase, allConfigurableTestCases)
import Debug.Trace(trace)

adventOfCode :: Eq b => Solution b => Parser a -> Computor a b -> String -> [TestCase b] -> IO ()
adventOfCode parseF computeF day cases = do
    results <- allTestCases parseF computeF day cases
    mapM_ (putStrLn . showTestResult) results
    putStrLn ""
    if testsPass results
        then do
            answer <- solve parseF computeF (day ++ "/input")
            putStrLn $ showSolution answer 
        else putStrLn "Test failure - please fix before attempting on full problem"
    return ()

adventOfConfigurableCode :: Eq b => Solution b => Parser a -> ConfigurableComputor c a b -> c -> String -> [ConfigurableTestCase c b] -> IO ()
adventOfConfigurableCode parseF computeF mainConfig day cases = do
    results <- allConfigurableTestCases parseF computeF day cases
    mapM_ (putStrLn . showTestResult) results
    putStrLn ""
    if testsPass results
        then do
            answer <- solve parseF (computeF mainConfig) (day ++ "/input")
            putStrLn $ showSolution answer
        else putStrLn "Test failure - please fix before attempting on full problem"
    return ()

noOp :: a -> Maybe b
noOp _ = Nothing

traceNoOp :: Show a => a -> Maybe b
traceNoOp p = trace (show p) Nothing
