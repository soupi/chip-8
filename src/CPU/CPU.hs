{-# LANGUAGE LambdaCase #-}

module CPU.CPU
where

import qualified Data.Word as W (Word8,Word16)
import qualified Data.Vector.Unboxed as V

-- |
-- Modeling a CHIP-8 CPU
data CPU = CPU { opcode       :: W.Word16
               , index        :: W.Word16
               , pc           :: W.Word16
               , sp           :: W.Word8
               , delayTimer   :: W.Word8
               , soundTimer   :: W.Word8
               , stack        :: V.Vector W.Word8
               , registers    :: V.Vector W.Word8
               , keypad       :: V.Vector W.Word8
               , memory       :: V.Vector W.Word8
               , gfx          :: V.Vector W.Word8
               } deriving (Read, Show, Eq)


-- |
-- initializing the CPU with 0
initCPU :: CPU
initCPU = CPU { opcode         = 0
              , index          = 0
              , pc             = 0
              , sp             = 0
              , delayTimer     = 0
              , soundTimer     = 0
              , stack          = V.replicate 16 0
              , registers      = V.replicate 16 0
              , keypad         = V.replicate 16 0
              , memory         = V.replicate 4096 0
              , gfx            = V.replicate (64 * 32) 0
              }

-- |
-- Returns the program counter as Int
getPC :: CPU -> Int
getPC = fromIntegral . pc

-- |
-- Returns the stack pointer as Int
getSP :: CPU -> Int
getSP = fromIntegral . sp
