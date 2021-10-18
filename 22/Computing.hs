module Computing (
    viablePairs,
) where
import Parsing (Node(..))

avail :: Node -> Int
avail (Node _ _ size used _) = size - used

viablePairs :: [Node] -> [(Node, Node)]
viablePairs l = [(a, b) | a <- l,
                          b <- l,
                          used a > 0,
                          a /= b,
                          used a <= avail b ]
