-- Bits Utilities

module CPU.Bits where

import Data.Word
import Data.Bits
import qualified Numeric as Nume (showHex)
import Data.Char (toUpper)



-- |
-- combining 2 Word8 to Word16
merge :: Word8 -> Word8 -> Word16
merge high low = shift (fromIntegral high) 8 .|. fromIntegral low

-- |
-- Formatting Word16 for debugging purposes
showHex :: Word16 -> String
showHex = ("0x"++) . map toUpper . flip Nume.showHex ""

-- |
-- a helper function for bit pattern matching
-- 16 bits becomes 32 bits because there isn't a 4 bit data type
match16 :: Word16 -> (Word8, Word8, Word8, Word8)
match16 n =
  (fromIntegral $ rotateL (n .&. 0xF000) 4
  ,fromIntegral $ rotateR (n .&. 0x0F00) 8
  ,fromIntegral $ rotateR (n .&. 0x00F0) 4
  ,fromIntegral (n .&. 0x000F)
  )
