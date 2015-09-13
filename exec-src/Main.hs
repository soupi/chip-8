{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Runtime.Run as Runtime (main)

main :: IO ()
main = Runtime.main

