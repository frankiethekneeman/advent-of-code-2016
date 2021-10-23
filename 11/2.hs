module Two where
import AoC (adventOfCode, noOp)
import Parsing (parseInput, State(..), Item(..))
import Computing (calculateStepsToSolve)
import Seqs (update)
import qualified Data.Set as Set

foundEquipment = Set.fromList $ [Generator, Microchip] <*> ["dilithium", "elerium"]

addFoundEquipment :: State -> State
addFoundEquipment (State curr floors) = State curr floors'
    where floors' = update (Set.union foundEquipment) 0 floors

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) second first input = second <$> first input

main = adventOfCode (addFoundEquipment <.> parseInput) calculateStepsToSolve "11" []
