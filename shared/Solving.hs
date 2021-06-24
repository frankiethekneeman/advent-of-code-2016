module Solving (
solve,
showSolution,
Parser,
Computor
) where

import MonadUtils (applyRight, orError)

type Parser a = (String -> Maybe a)
type Computor a b = (a -> Maybe b)

solve :: Parser a -> Computor a b -> String -> IO (Either String b)
solve parser computor fname = do
    input <- readFile fname
    pure (work parser computor input)


showSolution :: Show a => Either String a -> String
showSolution (Left err) = "Error: " ++ err
showSolution (Right val) = show val

--NOT EXPORTED

parse :: Parser a -> String -> Either String a
parse parser input = orError (parser input) "Failed to parse"

compute :: Computor a b -> a -> Either String b
compute computor parsed = orError (computor parsed) "Failed to Compute"

work :: (String -> Maybe a) -> (a -> Maybe b) -> String -> Either String b
work parser computor = (applyRight (compute computor)).(parse parser)
