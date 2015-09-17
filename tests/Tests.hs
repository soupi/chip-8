-- Tests

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

import qualified CPU.CPU as CPU
import CPU.Emulate
import CPU.Utils (replicateMChain)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
  [testCPU "jump"            jump514 3 0x202      $ CPU._pc
  ,testCPU "call subroutine" callSub 3 (0x200, 3) $ apply2 (CPU._pc, CPU._sp)
  ]

apply2 :: (a -> b, a -> c) -> a -> (b, c)
apply2 (f, g) x = (f x, g x)

testCPU :: (Show a, Eq a) => String -> BS.ByteString -> Int -> a -> (CPU.CPU -> a) -> TestTree
testCPU name program times result getFocus =
  testCase ("Testing " ++ name) $
    (pure . getFocus =<< replicateMChain times emulateCycle =<< loadGame program) @?= Right result

jump514 :: BS.ByteString
jump514 = BS.pack [0x00, 0xE0, 0x00, 0xE0, 0x12, 0x02]

callSub :: BS.ByteString
callSub = BS.pack [0x22, 0x00]



