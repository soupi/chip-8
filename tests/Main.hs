-- Tests

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit (Counts, Test(..), runTestTT, assertEqual)

import qualified Data.ByteString as BS
import qualified Lens.Micro.Mtl as Lens

import qualified CPU.CPU as CPU
import CPU.Emulate
import CPU.Utils (replicateMChain)

main :: IO Counts
main =
  runTestTT $
    TestList
      [testJump]


testJump :: Test
testJump =
  TestCase $
    assertEqual
      "Testing jump"
      (Right 0x200)
      (pure . Lens.view CPU.pc =<< replicateMChain 2 emulateCycle =<< loadGame jump512)

jump512 :: BS.ByteString
jump512 = BS.pack [0x12, 0x00]
