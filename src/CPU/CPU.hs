{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU.CPU
where

import qualified Data.Word as W (Word8,Word16)
import qualified Data.Vector.Unboxed as V
import Lens.Micro.TH (makeLenses)
import qualified Lens.Micro.Mtl as Lens (view)

-- |
-- Modeling a CHIP-8 CPU
data CPU = CPU { _opcode       :: W.Word16
               , _index        :: W.Word16
               , _pc           :: W.Word16
               , _sp           :: W.Word16
               , _delayTimer   :: W.Word8
               , _soundTimer   :: W.Word8
               , _stack        :: V.Vector W.Word16
               , _registers    :: V.Vector W.Word8
               , _keypad       :: V.Vector W.Word8
               , _memory       :: V.Vector W.Word8
               , _gfx          :: V.Vector W.Word8
               } deriving (Read, Show, Eq)

makeLenses ''CPU

-- |
-- initializing the CPU with 0
initCPU :: CPU
initCPU = CPU { _opcode         = 0
              , _index          = 0
              , _pc             = 0
              , _sp             = 0
              , _delayTimer     = 0
              , _soundTimer     = 0
              , _stack          = V.replicate 16 0
              , _registers      = V.replicate 16 0
              , _keypad         = V.replicate 16 0
              , _memory         = V.replicate 4096 0
              , _gfx            = V.replicate (64 * 32) 0
              }

-- |
-- Returns the program counter as Int
getPC :: CPU -> Int
getPC = fromIntegral . Lens.view pc

-- |
-- Returns the stack pointer as Int
getSP :: CPU -> Int
getSP = fromIntegral . Lens.view sp
