module Utils where

import Data.Word
import Text.Printf

word8ToInt :: Word8 -> Int
word8ToInt = fromInteger . toInteger

word16ToInt :: Word16 -> Int
word16ToInt = fromInteger . toInteger

showHex :: Word16 -> String
showHex = printf "0x%04x"

showHexWord8 :: Word8 -> String
showHexWord8 = printf "0x%02x"
