{-# LANGUAGE LambdaCase #-}

module CPU.Emulate where

-------------
-- Imports
-------------

import Control.Monad ((>=>), (<=<))
import qualified Data.Word as W (Word8, Word16)
import qualified Data.Vector.Unboxed as V
import Data.Bits ((.&.), (.|.), xor, shiftR, shiftL)
import Lens.Micro ((&))
import qualified Lens.Micro     as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Data.ByteString as BS

import Utils (supplyBoth)
import CPU.CPU (CPU, Error, throwErr, throwErrText)
import qualified CPU.CPU as CPU
import qualified CPU.Bits as Bits

import Debug.Trace

---------------
-- Emulation
---------------

loadGame :: BS.ByteString -> Emulate CPU
loadGame game =
  if BS.length game <= V.length (Lens.view CPU.memory cpu) - 0x0200
  then
    pure $ cpu & Lens.set  CPU.pc 0x200
               & Lens.over CPU.memory (`V.update` V.fromList (zip [0x200..] (BS.unpack game)))
  else
    throwErrText "Cannot fit game in memory"
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
      throwErr cpu "Program counter out of bounds"

  where cmd = Bits.merge16 <$> (Lens.view CPU.memory cpu V.!? CPU.getPC cpu) <*> (Lens.view CPU.memory cpu V.!? (CPU.getPC cpu + 1))

decode :: W.Word16 -> Emulate Instruction
decode cmd =
 case findOpcode cmd of
    Just instruction ->
      pure instruction
    Nothing ->
      throwErrText $ "Opcode: " ++ Bits.showHex16 cmd ++ " not implemented."


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
  then throwErr cpu "The stack is full"
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
    throwErr cpu "The stack is full"
  else
    pure (Lens.view CPU.stack cpu V.! CPU.getSP cpu, Lens.over CPU.sp (\x -> x-1) cpu)


-------------
-- Opcodes
-------------

-- |
-- finds the relevant instruction from opcode
findOpcode :: W.Word16 -> Maybe Instruction
findOpcode opcode =
  trace (Bits.showHex16 opcode) $
  case Bits.match16 opcode of
    (0x0, 0x0, 0xE, 0x0) ->
      pure (nextPC <=< clearScreen)
    (0x1, _, _, _) ->
      pure $ jump (opcode .&. 0x0FFF)
    (0x2, _, _, _) ->
      pure $ callSubroutine (opcode .&. 0x0FFF)
    (0x6, v, _, _) ->
      pure $ setRegister v $ fromIntegral (opcode .&. 0x00FF)
    (0x7, v, _, _) ->
      pure $ addToRegister v $ fromIntegral (opcode .&. 0x00FF)
    (0x8, x, y, 0x0) ->
      pure $ movRegister x y
    (0x8, x, y, 0x1) ->
      pure $ orRegisters x y
    (0x8, x, y, 0x2) ->
      pure $ andRegisters x y
    (0x8, x, y, 0x3) ->
      pure $ xorRegisters x y
    (0x8, x, y, 0x4) ->
      pure $ addRegisters x y
    (0x8, x, y, 0x5) ->
      pure $ subRegisters x y
    (0x8, x, _, 0x6) ->
      pure $ shiftRegisterR x
    (0x8, x, y, 0x7) ->
      pure $ subRegistersBackwards x y
    (0x8, x, _, 0xE) ->
      pure $ shiftRegisterL x
    (0xA, _, _, _) ->
      pure $ setIndex (opcode .&. 0x0FFF)
    (0xB, _, _, _) ->
      pure $ jumpPlusIndex (opcode .&. 0x0FFF)
    (0xF, x, 0x3, 0x3) ->
      pure $ storeBinRep x
    _ -> Nothing

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
-- Opcode 0xBnnn
-- Jump to address 0x0nnn + content of index register
-- changes the program counter
jumpPlusIndex :: W.Word16 -> Instruction
jumpPlusIndex address cpu =
  jump (address + Lens.view CPU.index cpu) cpu

-- |
-- Opcode 0xFx33
-- Write the binary-coded decimal of register x to memory
-- at addresses index, index+1 and index+2
-- Changes the memory
storeBinRep :: W.Word8 -> Instruction
storeBinRep x cpu =
  let
    (n1,n2,n3) = Bits.bcd8 $ Lens.view CPU.registers cpu V.! fromIntegral x
    (i1,i2,i3) = (\i -> (fromIntegral i, fromIntegral i+1, fromIntegral i+2)) (Lens.view CPU.index cpu)
 in
    pure $ Lens.over CPU.memory (V.// [(i1, n1), (i2, n2), (i3, n3)]) cpu


-- |
-- Opcode 0xAnnn
-- Sets the index register to the immediate value nnn
-- changes the index register
setIndex :: W.Word16 -> Instruction
setIndex address =
  pure . Lens.set CPU.index address

-- |
-- Opcode 0x6vnn
-- Sets the register v to the immediate value nn
-- changes the register v
setRegister :: W.Word8 -> W.Word8 -> Instruction
setRegister regNum value =
  pure . Lens.over CPU.registers (V.// [(fromIntegral regNum, value)])

-- |
-- Opcode 0x7vnn
-- Add the immediate value nn to the register v
-- changes the register v
addToRegister :: W.Word8 -> W.Word8 -> Instruction
addToRegister regNum value cpu =
  setRegister regNum (value + vx) cpu
  where vx = CPU.regVal regNum cpu


-- |
-- Opcode 0x8xy0
-- Sets the register x to the content of register y
-- changes the register x
movRegister :: W.Word8 -> W.Word8 -> Instruction
movRegister = binOpRegisters (flip const)

-- |
-- Opcode 0x8xy1
-- Sets the register x to (x | y)
-- changes the register x
orRegisters :: W.Word8 -> W.Word8 -> Instruction
orRegisters = binOpRegisters (.|.)


-- |
-- Opcode 0x8xy2
-- Sets the register x to (x & y)
-- changes the register x
andRegisters :: W.Word8 -> W.Word8 -> Instruction
andRegisters = binOpRegisters (.&.)

-- |
-- Opcode 0x8xy3
-- Sets the register x to (x `xor` y)
-- changes the register x
xorRegisters :: W.Word8 -> W.Word8 -> Instruction
xorRegisters = binOpRegisters xor


-- |
-- Opcode 0x8xy4
-- Adds the registers x and y and stores them at register x. sets carry in register F
-- changes the registers x and F
addRegisters :: W.Word8 -> W.Word8 -> Instruction
addRegisters x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vx + vy), (0xF, carry)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        carry = if (0xFF :: W.Word16) < fromIntegral vx + fromIntegral vy then 1 else 0

-- |
-- Opcode 0x8xy5
-- Subtract the registers y from x and store in x. sets borrow in register F
-- changes the registers x and F
subRegisters :: W.Word8 -> W.Word8 -> Instruction
subRegisters x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vx - vy), (0xF, borrow)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        borrow = if vx < vy then 0 else 1


-- |
-- Opcode 0x8x_6
-- Shifts register x right by 1 and sets register F to the value
-- of the LSB of reg x before the shift
shiftRegisterR :: W.Word8 -> Instruction
shiftRegisterR = shiftRegister shiftR 0x0F

-- |
-- Opcode 0x8xy7
-- Subtract the registers x from y and store in x. sets borrow in register F
-- changes the registers x and F
subRegistersBackwards :: W.Word8 -> W.Word8 -> Instruction
subRegistersBackwards x y cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral x, vy - vx), (0xF, borrow)]) cpu
  where vx = CPU.regVal x cpu
        vy = CPU.regVal y cpu
        borrow = if vx > vy then 0 else 1



-- |
-- Opcode 0x8x_E
-- Shifts register x left by 1 and sets register F to the value
-- of the MSB of reg x before the shift
shiftRegisterL :: W.Word8 -> Instruction
shiftRegisterL = shiftRegister shiftL 0xF0 -- MIGHT NOT BE OK



-- |
-- Shifts register x by 1 and sets register F to the value
-- of a significant bit of reg x before the shift
-- changes the registers x and F
shiftRegister :: (W.Word8 -> Int -> W.Word8) -> W.Word8 -> W.Word8 -> Instruction
shiftRegister shiftType compareParam regNum cpu =
  pure $ Lens.over CPU.registers (V.// [(fromIntegral regNum, shiftType 1 $ fromIntegral vx), (0XF, lsb)]) cpu
  where vx  = CPU.regVal regNum cpu
        lsb = vx .&. compareParam


-- |
-- Sets the register x to (x `op` y)
-- changes the register x
binOpRegisters :: (W.Word8 -> W.Word8 -> W.Word8) -> W.Word8 -> W.Word8 -> Instruction
binOpRegisters op x y cpu =
  setRegister x (CPU.regVal x cpu `op` CPU.regVal y cpu) cpu


