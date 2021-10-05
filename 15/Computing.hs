module Computing (
    firstPerfectDrop,
) where
import Parsing(Disk(..))
import CRT(CongruenceClass(..), reduce, combineAll)

firstPerfectDrop :: [Disk] -> Maybe Integer
firstPerfectDrop disks = remainder <$> result
    where result = combineAll classes
          classes = map dropTimes disks

dropTimes :: Disk -> CongruenceClass
dropTimes (Disk n p o) = reduce $ CongruenceClass (-(n + o)) p
