{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector.Unboxed as V
-- import Data.Bits ((.&.))
import Lens.Micro ((&))
import qualified Lens.Micro     as Lens
import qualified Lens.Micro.Mtl as Lens

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

  where cmd = Bits.merge <$> (Lens.view CPU.memory cpu V.!? CPU.getPC cpu) <*> (Lens.view CPU.memory cpu V.!? (CPU.getPC cpu + 1))

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
  cpu & Lens.over CPU.delayTimer (\dt -> if dt > 0 then dt - 1 else 0)
      & Lens.over CPU.soundTimer (\st -> if st > 0 then st - 1 else 0)

----------------
-- update CPU
----------------

-- |
-- increases the program counter by 2
nextPC :: CPU -> Emulate CPU
nextPC cpu =
  pure $ Lens.over CPU.pc (+2) cpu

-- |
-- store a value on the stack, updating the stack and the stack pointer.
-- fail if out of bounds
storeOnStack :: W.Word8 -> CPU -> Emulate CPU
storeOnStack val cpu =
  if CPU.getSP cpu >= V.length (Lens.view CPU.stack cpu)
  then throwErr "The stack is full"
  else pure $
    cpu & Lens.over CPU.stack (V.// [(CPU.getSP cpu, val)])
        & Lens.over CPU.sp (+1)

-- |
-- pops a value from the stack, updating the stack pointer.
-- fail if out of bounds
popFromStack :: CPU -> Emulate (W.Word8, CPU)
popFromStack cpu =
  if CPU.getSP cpu <= 0 && CPU.getSP cpu >= V.length (Lens.view CPU.stack cpu)
  then
    throwErr "The stack is full"
  else
    pure (Lens.view CPU.stack cpu V.! CPU.getSP cpu, Lens.over CPU.sp (\x -> x-1) cpu)


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

