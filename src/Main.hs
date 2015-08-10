{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS

import CPU.Emulate

main :: IO ()
main = do
  putStrLn "Hello, CHIP-8!"
  print $ emulateCycle =<< emulateCycle =<< loadGame gameData

gameData :: BS.ByteString
gameData = BS.pack [0x00, 0xE0, 0x12, 0x00]
