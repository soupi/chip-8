-- Tests

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Random as Rand
import qualified Data.ByteString as BS

import qualified CPU.CPU as CPU
import CPU.Emulate
import CPU.Utils (replicateMChain)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
  [testCPU "jump"            jump514 3 0x202        CPU._pc
  ,testCPU "call subroutine" callSub 3 (0x200, 3) $ apply2 (CPU._pc, CPU._sp)
  ,testCPU "collision"       collide 4 0x1        $ CPU.regVal 0xF
  ,testCPU "no collision"    noColl  7 0x0        $ CPU.regVal 0xF
  ,testCPU "Shift Right "    shftR   2 (0x1, 0x1) $ apply2 (CPU.regVal 0x1, CPU.regVal 0xF)
  ]

apply2 :: (a -> b, a -> c) -> a -> (b, c)
apply2 (f, g) x = (f x, g x)

testCPU :: (Show a, Eq a) => String -> BS.ByteString -> Int -> a -> (CPU.CPU -> a) -> TestTree
testCPU name program times result getFocus =
  testCase ("Testing " ++ name) $
    (pure . getFocus =<< replicateMChain times emulateCycle =<< loadGameAndFonts (Rand.mkStdGen 100) program) @?= Right result

jump514 :: BS.ByteString
jump514 = BS.pack [0x00, 0xE0, 0x00, 0xE0, 0x12, 0x02]

callSub :: BS.ByteString
callSub = BS.pack [0x22, 0x00]

collide :: BS.ByteString
collide =
  BS.pack [0xF1, 0x29
          ,0xD0, 0x05
          ,0xF1, 0x29
          ,0xD0, 0x05
          ]

noColl :: BS.ByteString
noColl =
  BS.pack [0x61, 0x01
          ,0xF1, 0x29
          ,0xD0, 0x05
          ,0xF1, 0x29
          ,0xD0, 0x05
          ,0xF1, 0x29
          ,0x60, 0x0F
          ,0xD0, 0x05
          ]

shftR :: BS.ByteString
shftR =
  BS.pack [0x61, 0x03
          ,0x81, 0x06
          ]


