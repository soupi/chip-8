-- Bits Utilities

module CPU.Bits where

import Data.Word
import Data.Bits
import qualified Numeric as Nume (showHex)
import Data.Char (toUpper)



-- |
-- combining 2 Word8 to Word16
merge16 :: Word8 -> Word8 -> Word16
merge16 high low = shift (fromIntegral high) 8 .|. fromIntegral low

-- |
-- combining 2 Word8 to Word8
merge8 :: Word8 -> Word8 -> Word8
merge8 high low = shift (fromIntegral (word8to4 high)) 4 .|. fromIntegral (word8to4 low)

word8to4 :: Word8 -> Word8
word8to4 = (.&.) 0xF

-- |
-- Formatting Word16 for debugging purposes
showHex16 :: Word16 -> String
showHex16 = ("0x"++) . (\f (w,x,y,z) -> f w ++ f x ++ f y ++ f z) (map toUpper . flip Nume.showHex "") . match16

-- |
-- Formatting Word8 for debugging purposes
showHex8 :: Word8 -> String
showHex8 n =
  "0x" ++
  (\f (x,y) -> f x ++ f y) (map toUpper . flip Nume.showHex "")
  (rotateR (n .&. 0xF0) 4, n .&. 0x0F)

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

bcd8 :: Word8 -> (Word8, Word8, Word8)
bcd8 n =
  case map (read . (:[])) (show n) of
    [z]     -> (0,0,z)
    [y,z]   -> (0,y,z)
    [x,y,z] -> (x,y,z)
    _ -> error $ "Impossible pattern match for: " ++ show n
