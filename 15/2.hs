module Two where
import AoC (adventOfCode)
import Parsing(readDisks, Disk(..))
import Computing(firstPerfectDrop)

addDisk :: [Disk] -> [Disk]
addDisk disks = disks ++ [Disk (n + 1) 11 0]
    where n = foldl max 0 $ map num disks

main = adventOfCode readDisks (firstPerfectDrop . addDisk ) "15" []
