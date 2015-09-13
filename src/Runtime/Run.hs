{-# LANGUAGE OverloadedStrings #-}

module Runtime.Run (main) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import qualified SDL
import qualified Linear

import qualified MySDL.MySDL as MySDL
import qualified CPU.CPU as CPU
import           CPU.CPU   (CPU)
import           CPU.Emulate
import           CPU.Utils (mapLeft)

import Debug.Trace

main :: IO ()
main = do
  putStrLn "Hello, CHIP-8!"
  case loadGame gameData of
    Left (_, err) -> putStrLn $ "Could not load game.\nError: " ++ err
    Right result  -> run result

gameData :: BS.ByteString
gameData =
  BS.pack
    [0x00, 0xE0
    ,0x70, 0x01
    ,0x12, 0x00
    ]

run :: CPU -> IO ()
run world =
  MySDL.withWindow "CHIP-8" (MySDL.myWindowConfig (Linear.V2 800 600)) $
    flip MySDL.withRenderer (MySDL.apploop world update . render)

update :: MonadIO m => [SDL.Event] -> CPU -> m (Either (Maybe String) CPU)
update _ = pure . mapLeft (pure . CPU.showErr) . emulateCycle

render :: MonadIO m => (SDL.Window, SDL.Renderer) -> CPU -> m ()
render (_, renderer) cpu =
  SDL.present =<< MySDL.setBGColor (Linear.V4 (fromIntegral $ CPU.regVal 0 cpu) 180 230 180) renderer

