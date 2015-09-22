module CPU.Disassembler where

import Data.Bits
import qualified CPU.Bits as Bits
import qualified Data.Word as W (Word16)

-- |
-- finds the relevant instruction from opcode.
-- Based on this: https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
showOpcode :: W.Word16 -> String
showOpcode opcode =
  case Bits.match16 opcode of
    (0x0, 0x0, 0xE, 0x0) ->
      "CLR"
    (0x0, 0x0, 0xE, 0xE) ->
      -- "Returns from a subroutine."
      "RET"
    (0x0, _, _, _) ->
      -- "Calls RCA 1802 program at address NNN. Not necessary for most ROMs." - not yet implemented
      Bits.showHex16 opcode ++ " - not implemented"
    (0x1, _, _, _) ->
      "JUMP " ++ Bits.showHex16 (opcode .&. 0x0FFF)
    (0x2, _, _, _) ->
      "CALL " ++ Bits.showHex16 (opcode .&. 0x0FFF)
    (0x3, reg, _, _) ->
      "SKIF== @" ++ Bits.showHex8 reg ++ ", " ++ Bits.showHex16 (opcode .&. 0x00FF)
    (0x4, reg, _, _) ->
      "SKIF/= @" ++ Bits.showHex8 reg ++ ", " ++ Bits.showHex16 (opcode .&. 0x00FF)
    (0x5, reg1, reg2, 0x0) ->
      "SKIF== @" ++ Bits.showHex8 reg1 ++ ", @" ++ Bits.showHex8 reg2
    (0x6, v, _, _) ->
      "SET " ++ Bits.showHex8 v ++ ", " ++ Bits.showHex16 (opcode .&. 0x00FF)
    (0x7, v, _, _) ->
      "ADD " ++ Bits.showHex8 v ++ ", " ++ Bits.showHex16 (opcode .&. 0x00FF)
    (0x8, x, y, 0x0) ->
      "MOV " ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, y, 0x1) ->
      "OR @" ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, y, 0x2) ->
      "AND @" ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, y, 0x3) ->
      "XOR @" ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, y, 0x4) ->
      "ADD " ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, y, 0x5) ->
      "SUB " ++ Bits.showHex8 x ++ ", @" ++ Bits.showHex8 y
    (0x8, x, _, 0x6) ->
      "SHR " ++ Bits.showHex8 x
    (0x8, x, y, 0x7) ->
      "SUB-> @" ++ Bits.showHex8 x ++ ", " ++ Bits.showHex8 y
    (0x8, x, _, 0xE) ->
      "SHL " ++ Bits.showHex8 x
    (0x9, reg1, reg2, 0x0) ->
      "SKIF/= @" ++ Bits.showHex8 reg1 ++ ", @" ++ Bits.showHex8 reg2
    (0xA, _, _, _) ->
      "SETIX " ++ Bits.showHex16 (opcode .&. 0x0FFF)
    (0xB, _, _, _) ->
      "JUMP+IX " ++ Bits.showHex16 (opcode .&. 0x0FFF)
    (0xC, reg, _, _) ->
      "SET-RAND " ++ Bits.showHex8 reg ++ ", " ++ Bits.showHex16 (opcode .&. 0x00FF)
    (0xD, reg1, reg2, times) ->
      -- "Sprites stored in memory at location in index register (I), 8bits wide. Wraps around the screen. If when drawn, clears a pixel, register VF is set to 1 otherwise it is zero. All drawing is XOR drawing (i.e. it toggles the screen pixels). Sprites are drawn starting at position VX, VY. N is the number of 8bit rows that need to be drawn. If N is greater than 1, second line continues at position VX, VY+1, and so on." -- not yet implemented
      "DRAW " ++ Bits.showHex8 reg1 ++ ", " ++ Bits.showHex8 reg2 ++ ", " ++ Bits.showHex8 times
    (0xE, reg, 0x9, 0xE) ->
      -- "Skips the next instruction if the key stored in VX is pressed."
      "SKIF==KEY @" ++ Bits.showHex8 reg
    (0xE, reg, 0xA, 0x1) ->
      -- "Skips the next instruction if the key stored in VX isn't pressed."
      "SKIF/=KEY @" ++ Bits.showHex8 reg
    (0xF, reg, 0x0, 0x7) ->
      "SET-DELAY " ++ Bits.showHex8 reg
    (0xF, reg, 0x0, 0xA) ->
      -- "A key press is awaited, and then stored in VX."
      "WAIT " ++ Bits.showHex8 reg
    (0xF, reg, 0x1, 0x5) ->
      "SET-DT @" ++ Bits.showHex8 reg
    (0xF, reg, 0x1, 0x8) ->
      "SET-ST @" ++ Bits.showHex8 reg
    (0xF, reg, 0x1, 0xE) ->
      "ADD-IX @" ++ Bits.showHex8 reg
    (0xF, reg, 0x2, 0x9) ->
      -- "Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font."
      "SET-IX-SPRITE @" ++ Bits.showHex8 reg
    (0xF, reg, 0x3, 0x3) ->
      "STORE-BIN-REP " ++ Bits.showHex8 reg
    (0xF, reg, 0x5, 0x5) ->
      "STORE-REG " ++ Bits.showHex8 reg
    (0xF, reg, 0x6, 0x5) ->
      "STORE-MEM " ++ Bits.showHex8 reg
    _ ->
      "UNKOWN: " ++ Bits.showHex16 opcode


