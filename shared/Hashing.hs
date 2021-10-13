module Hashing (
    md5String
) where
import Distribution.Utils.MD5(md5, showMD5)
import Data.ByteString.Internal (packChars)

md5String :: String -> String
md5String = showMD5 . md5 . packChars
