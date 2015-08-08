module Main where

import CPU.CPU
import CPU.Emulate

main :: IO ()
main = do
  putStrLn "Hello, CHIP-8!"
  print $ emulateCycle initCPU
