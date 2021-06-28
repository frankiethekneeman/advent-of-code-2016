module Computing (
getInterestingHashes
) where

import Distribution.Utils.MD5(md5, showMD5)
import Data.List(isPrefixOf)
import Data.ByteString.Internal (packChars)

getInterestingHashes :: String -> [String]
getInterestingHashes id = map (drop toDrop) . filter (isPrefixOf interestingPrefix) $ map genHash [0..]
    where genHash = showMD5 . md5 . packChars . (id ++) . show :: Int -> String

interestingPrefix = "00000"
toDrop = length interestingPrefix
