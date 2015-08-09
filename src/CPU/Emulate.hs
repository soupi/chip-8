{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import Control.Monad ((>=>), (<=<))
import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector.Unboxed as V
import Data.Bits ((.&.))
import Lens.Micro ((&))
import qualified Lens.Micro     as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Data.ByteString as BS

import Utils (Error, throwErr, supplyBoth)
import CPU.CPU (CPU)
import qualified CPU.CPU as CPU
import qualified CPU.Bits as Bits

---------------
-- Emulation
---------------

loadGame :: BS.ByteString -> Emulate CPU
loadGame game =
  if BS.length game <= V.length (Lens.view CPU.memory cpu) - 0x0200
  then
    pure $ Lens.over CPU.memory (`V.update` V.fromList (zip [0x200..] (BS.unpack game))) cpu
  else
    throwErr "Cannot fit game in memory"
  where cpu = CPU.initCPU

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

  where cmd = Bits.merge16 <$> (Lens.view CPU.memory cpu V.!? CPU.getPC cpu) <*> (Lens.view CPU.memory cpu V.!? (CPU.getPC cpu + 1))

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
storeOnStack :: W.Word16 -> CPU -> Emulate CPU
storeOnStack val cpu =
  if CPU.getSP cpu >= V.length (Lens.view CPU.stack cpu)
  then throwErr "The stack is full"
  else pure $
    cpu & Lens.over CPU.stack (V.// [(CPU.getSP cpu, val)])
        & Lens.over CPU.sp (+1)

-- |
-- pops a value from the stack, updating the stack pointer.
-- fail if out of bounds
popFromStack :: CPU -> Emulate (W.Word16, CPU)
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
    (0x0, 0x0, 0xE, 0x0) ->
      pure (nextPC <=< clearScreen)
    (0x2, _, _, _) ->
      pure $ callSubroutine (opcode .&. 0x0FFF)
    (0x1, _, _, _) ->
      pure $ jump (opcode .&. 0x0FFF)
    (0x8, x, y, 4) ->
      pure $ addRegisters x y
    (0xF, x, 3, 3) ->
      pure $ storeBinRep x


-- |
-- Opcode 0x00E0
-- Clears the screen
clearScreen :: Instruction
clearScreen =
  pure . Lens.over CPU.gfx (V.map (const 0))

-- |
-- Opcode 0x1nnn
-- Jump to address 0x0nnn
-- changes the program counter to given address
jump :: W.Word16 -> Instruction
jump address =
  pure . Lens.set CPU.pc address

-- |
-- Opcode 0x2nnn
-- Call a subroutine on address 0x0nnn
-- Saves the program counter on the stack and changes it to given address
callSubroutine :: W.Word16 -> Instruction
callSubroutine address =
  supplyBoth storeOnStack (Lens.view CPU.pc) >=> jump address

-- |
-- Opcode 0x8xy4
-- Adds the registers x and y. sets carry in register F
-- changes the registers x, y and F
addRegisters :: W.Word8 -> W.Word8 -> Instruction
addRegisters x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vx + vy), (0xF, carry)]) cpu
  where vx = Lens.view CPU.registers cpu V.! fromIntegral x
        vy = Lens.view CPU.registers cpu V.! fromIntegral y
        carry = if (0xFF :: W.Word16) < fromIntegral vx + fromIntegral vy then 1 else 0

-- |
-- Opcode 0xFx33
-- Write the binary-coded decimal of register x to memory
-- at addresses index, index+1 and index+2
-- Changes the memory
storeBinRep :: W.Word8 -> Instruction
storeBinRep x cpu =
  let
    (n1,n2,n3) = Bits.bcd8 $ Lens.view CPU.registers cpu V.! (fromIntegral x)
    (i1,i2,i3) = (\i -> (fromIntegral i, fromIntegral i+1, fromIntegral i+2)) (Lens.view CPU.index cpu)
 in
    pure $ Lens.over CPU.memory (V.// [(i1, n1), (i2, n2), (i3, n3)]) cpu


