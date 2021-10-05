module Solving (
solve,
showSolution,
Parser,
Computor,
ConfigurableComputor,
Solution,
toString
) where

import MonadUtils (applyRight, orError)

type Parser a = (String -> Maybe a)
type Computor a b = (a -> Maybe b)
type ConfigurableComputor a b c = (a -> Computor b c)

solve :: Parser a -> Computor a b -> String -> IO (Either String b)
solve parser computor fname = do
    input <- readFile fname
    pure (work parser computor input)


showSolution :: Solution a => Either String a -> String
showSolution (Left err) = "Error: " ++ err
showSolution (Right val) = toString val

--NOT EXPORTED
class Solution a where
    toString :: a -> String

instance {-# OVERLAPPING #-} Solution String where
    toString = id

instance Show a => Solution a where
    toString = show

parse :: Parser a -> String -> Either String a
parse parser input = orError (parser input) "Failed to parse"

compute :: Computor a b -> a -> Either String b
compute computor parsed = orError (computor parsed) "Failed to Compute"

work :: (String -> Maybe a) -> (a -> Maybe b) -> String -> Either String b
work parser computor = (applyRight (compute computor)).(parse parser)
