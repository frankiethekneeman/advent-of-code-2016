module Testing (
allTestCases,
showTestResult,
testsPass,
TestCase,
TestResult
) where
import Solving (solve, Parser, Computor, Solution, toString)
import MonadUtils (applyCollapse)

type TestCase a = (String, a)
type TestResult = (String, Maybe String)

allTestCases :: Solution b => Eq b => Parser a -> Computor a b -> String -> [TestCase b] -> IO [TestResult]
allTestCases parser computor dir cases = sequence $ map getResult cases
    where resolve fname = dir ++ "/ex" ++ fname
          execute fname = executeTestCase parser computor (resolve fname)
          getResult (fname, expected) = fmap (\result -> (fname, result)) (execute fname expected)

showTestResult :: TestResult -> String
showTestResult (testName, Nothing) = "Test " ++ testName ++ " Passed."
showTestResult (testName, Just err) = "Failed Test " ++ testName ++ ": " ++ err

testsPass :: [TestResult] -> Bool
testsPass [] = True
testsPass ((_, (Just _)):_) = False
testsPass ((_, Nothing):rest) = testsPass rest

-- NOT EXPORTED

executeTestCase :: Solution b => Eq b => Parser a -> Computor a b -> String -> b -> IO (Maybe String)
executeTestCase parser computor fname expected = do
    output <- solve parser computor fname
    pure $ applyCollapse (verify expected) output


verify :: Solution a => Eq a => a -> a -> Maybe String
verify expected actual
    | expected == actual = Nothing
    | otherwise = Just ("Expected " ++ (toString expected) ++ " but got " ++ (toString actual))

