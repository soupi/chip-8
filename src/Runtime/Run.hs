{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime.Run (main, runGame) where

import           System.Environment (getArgs)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import qualified SDL
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Lens.Micro.Mtl as Lens

import qualified MySDL.MySDL as MySDL
import qualified CPU.CPU as CPU
import           CPU.CPU   (CPU)
import           CPU.Emulate
import           CPU.Utils (mapLeft, map2)
import           Examples.Programs

main :: IO ()
main =
  getArgs >>= \case
    [file] ->
      BS.readFile file >>= runGame
    _ ->
      runGame gameDataBAD

runGame :: BS.ByteString -> IO ()
runGame gameData = do
 case loadGameAndFonts gameData of
    Left (_, err) ->
      putStrLn $ "Could not load game.\nError: " ++ err
    Right result -> do
      putStrLn "Hello CHIP-8!"
      run result

run :: CPU -> IO ()
run world =
  MySDL.withWindow "CHIP-8" (MySDL.myWindowConfig (Linear.V2 512 256)) $
    flip MySDL.withRenderer (MySDL.apploop world update . render)

update :: MonadIO m => [SDL.Event] -> CPU -> m (Either (Maybe String) CPU)
update _ = pure . mapLeft (pure . CPU.showErr) . emulateCycle

render :: MonadIO m => (SDL.Window, SDL.Renderer) -> CPU -> m ()
render (_, renderer) cpu = do
  MySDL.setBGColor (Linear.V4 (fromIntegral $ CPU.regVal 1 cpu) 180 230 180) renderer
  drawRects (Lens.view CPU.gfx cpu) renderer
  SDL.present renderer

squareSize :: C.CInt
squareSize =  8

convertGFX :: V.Vector Bool -> V.Vector (SDL.Rectangle C.CInt)
convertGFX gfx = V.map f $ V.filter snd $ V.indexed gfx
    where f (index, _)  = SDL.Rectangle (Linear.P $ uncurry Linear.V2 (determinePos $ fromIntegral index)) (Linear.V2 squareSize squareSize)
          determinePos i = map2 (* squareSize) (i `mod` 64, i `div` 64)

drawRects :: MonadIO m => V.Vector Bool -> SDL.Renderer -> m ()
drawRects gfx renderer = do
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 0 0 0 1
  let newGfx = convertGFX gfx
      rects  = VS.generate (V.length newGfx) (newGfx V.!)
  SDL.drawRects renderer rects
  SDL.fillRects renderer rects

