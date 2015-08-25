{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS

import CPU.Emulate
import CPU.Utils (replicateMChain)

main :: IO ()
main = do
  putStrLn "Hello, CHIP-8!"
  print $ replicateMChain 2 emulateCycle =<< loadGame gameData

gameData :: BS.ByteString
gameData = BS.pack [0x00, 0xE0, 0x12, 0x00]
