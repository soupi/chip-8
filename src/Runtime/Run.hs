{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime.Run (main, runGame) where

import           System.Environment (getArgs)
import qualified System.Random as Rand
import           Control.Monad.IO.Class (MonadIO)-- , liftIO)
import           Control.Monad          ((>=>), (<=<))
import qualified Data.ByteString as BS
import qualified SDL
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Lens.Micro.Mtl as Lens
import qualified Lens.Micro     as Lens

import qualified MySDL.MySDL as MySDL
--import qualified CPU.Bits as Bits
--import qualified CPU.Disassembler as DA
import qualified CPU.CPU as CPU
import           CPU.CPU   (CPU)
import           CPU.Emulate
import           CPU.Utils (mapLeft, map2, replicateMChain)

main :: IO ()
main =
  getArgs >>= \case
    [file] ->
      BS.readFile file >>= runGame
    _ ->
      putStrLn "Usage: chip8 <rom>"

runGame :: BS.ByteString -> IO ()
runGame gameData =
  case loadGameAndFonts (Rand.mkStdGen 100) gameData of
    Left (_, err) ->
      putStrLn $ "Could not load game.\nError: " ++ err
    Right result -> do
      putStrLn "Hello CHIP-8!"
      _ <- run result
      putStrLn "Goodbye."

run :: CPU -> IO CPU
run world =
  MySDL.withWindow "CHIP-8" (MySDL.myWindowConfig (Linear.V2 512 256)) $
    flip MySDL.withRenderer (setBGColor >=> MySDL.apploop world update . render)

update :: MonadIO m => [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> CPU -> m (Either (Maybe String) CPU)
update _ keysState = pure . mapLeft (pure . CPU.showErr) . (pure . updateTimers <=< replicateMChain 10 emulateCycle) . setKeys keysState . CPU.clearKeys


setKeys :: (SDL.Scancode -> Bool) -> CPU -> CPU
setKeys isKeyPressed =
  Lens.over CPU.keypad
  (V.//
     map (fmap isKeyPressed) keyMapping)

keyMapping :: [(Int, SDL.Scancode)]
keyMapping =
  [(0x1, SDL.Scancode1)
  ,(0x2, SDL.Scancode2)
  ,(0x3, SDL.Scancode3)
  ,(0x4, SDL.ScancodeQ)
  ,(0x5, SDL.ScancodeW)
  ,(0x6, SDL.ScancodeE)
  ,(0x7, SDL.ScancodeA)
  ,(0x8, SDL.ScancodeS)
  ,(0x9, SDL.ScancodeD)
  ,(0x0, SDL.ScancodeX)

  ,(0xa, SDL.ScancodeZ)
  ,(0xb, SDL.ScancodeC)
  ,(0xc, SDL.Scancode4)
  ,(0xd, SDL.ScancodeR)
  ,(0xe, SDL.ScancodeF)
  ,(0xf, SDL.ScancodeV)
  ]


setBGColor :: MonadIO m => (SDL.Window, SDL.Renderer) -> m (SDL.Window, SDL.Renderer)
setBGColor sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff


render :: MonadIO m => (SDL.Window, SDL.Renderer) -> CPU -> m ()
render (_, renderer) cpu = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  drawRects (Lens.view CPU.gfx cpu) renderer
  SDL.present renderer
--  case fetch cpu of -- show instructions for debug purposes
--    Left _   -> pure ()
--    Right op -> do
--      liftIO $ putStr (Bits.showHex16 $ fromIntegral (CPU.getPC cpu))
--      liftIO $ putStrLn $ ": " ++ DA.showOpcode op

squareSize :: C.CInt
squareSize =  8

convertGFX :: V.Vector Bool -> V.Vector (SDL.Rectangle C.CInt)
convertGFX gfx = V.map f $ V.filter snd $ V.indexed gfx
    where f (index, _)  = SDL.Rectangle (Linear.P $ uncurry Linear.V2 (determinePos $ fromIntegral index)) (Linear.V2 squareSize squareSize)
          determinePos i = map2 (* squareSize) (i `mod` 64, i `div` 64)

drawRects :: MonadIO m => V.Vector Bool -> SDL.Renderer -> m ()
drawRects gfx renderer = do
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  let newGfx = convertGFX gfx
      rects  = VS.generate (V.length newGfx) (newGfx V.!)
  SDL.drawRects renderer rects
  SDL.fillRects renderer rects

