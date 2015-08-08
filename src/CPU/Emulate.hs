{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector.Unboxed as V
-- import Data.Bits ((.&.))

import Utils
import CPU.CPU (CPU)
import qualified CPU.CPU as CPU
import qualified CPU.Bits as Bits

---------------
-- Emulation
---------------

loadGame :: String -> CPU
loadGame = undefined

type Emulate a = Either Error a

type Instruction = (CPU -> Emulate CPU)

emulateCycle :: CPU -> Emulate CPU
emulateCycle cpu = pure . updateTimers =<< execute cpu =<< decode =<< fetch cpu

fetch :: CPU -> Either Error W.Word16
fetch cpu =
  case cmd of
    Just instruction ->
      pure instruction
    Nothing ->
      throwErr "Program counter out of bounds"

  where cmd = Bits.merge <$> (CPU.memory cpu V.!? CPU.getPC cpu) <*> (CPU.memory cpu V.!? (CPU.getPC cpu + 1))

decode :: W.Word16 -> Emulate Instruction
decode cmd =
 case findOpcode cmd of
    Just instruction ->
      pure instruction
    Nothing ->
      throwErr $ "Could not find opcode: " ++ show cmd


execute :: a -> (a -> b) -> b
execute = flip ($)

updateTimers :: CPU -> CPU
updateTimers cpu =
  cpu { CPU.delayTimer = if CPU.delayTimer cpu > 0 then CPU.delayTimer cpu - 1 else 0
      , CPU.soundTimer = if CPU.soundTimer cpu > 0 then CPU.soundTimer cpu - 1 else 0
      }

----------------
-- update CPU
----------------

-- |
-- increases the program counter by 2
nextPC :: CPU -> Emulate CPU
nextPC cpu =
  pure cpu { CPU.pc = CPU.pc cpu + 2 }

-- |
-- store a value on the stack, updating the stack and the stack pointer.
-- fail if out of bounds
storeOnStack :: W.Word8 -> CPU -> Emulate CPU
storeOnStack val cpu =
  if CPU.getSP cpu >= V.length (CPU.stack cpu)
  then throwErr "The stack is full"
  else pure
    cpu { CPU.stack = CPU.stack cpu V.// [(CPU.getSP cpu, val)]
        , CPU.sp = CPU.sp cpu + 1
        }

-- |
-- pops a value from the stack, updating the stack pointer.
-- fail if out of bounds
popFromStack :: CPU -> Emulate (W.Word8, CPU)
popFromStack cpu =
  if CPU.getSP cpu <= 0 && CPU.getSP cpu >= V.length (CPU.stack cpu)
  then
    throwErr "The stack is full"
  else
    pure (CPU.stack cpu V.! CPU.getSP cpu, cpu { CPU.sp = CPU.sp cpu - 1 })


-------------
-- Opcodes
-------------

-- |
-- finds the relevant instruction from opcode
findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode =
  case Bits.match16 opcode of
    (0x0, 0x0, 0xE, 0x0) -> pure $ nextPC . clearScreen

-- |
-- Opcode 0x00E0
-- Clears the screen
clearScreen :: CPU -> CPU
clearScreen = undefined

