-- MySDL: some wrappers and utility functions around SDL

{-# LANGUAGE LambdaCase #-}

module MySDL.MySDL where

import Data.Word (Word8)
import Data.Text (Text)
import Control.Monad (when)
import qualified Data.Word as Word
import qualified Foreign.C.Types as C
import qualified SDL
import qualified Linear

-- |Config window
myWindowConfig :: Linear.V2 C.CInt -> SDL.WindowConfig
myWindowConfig size = SDL.defaultWindow { SDL.windowInitialSize = size }

-- |will init SDL and create a Window and pass in as a parameter to function
withWindow :: Text -> SDL.WindowConfig -> (SDL.Window -> IO a) -> IO a
withWindow title winConf go = do
  SDL.initializeAll

  window <- SDL.createWindow title winConf
  SDL.showWindow window

  result <- go window

  SDL.destroyWindow window
  SDL.quit

  pure result

-- |create a Surface and pass in as a parameter to function
withRenderer :: SDL.Window -> ((SDL.Window, SDL.Renderer) -> IO a) -> IO a
withRenderer window go = do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  go (window, renderer)


-- |app loop: takes the current world and functions that updates the world renders it
-- manage ticks, events and loop
apploop :: a -> ([SDL.EventPayload] -> (SDL.Scancode -> Bool) -> a -> IO (Either (Maybe String) a)) -> (a -> IO ()) -> IO a
apploop world update render = do
  tick <- SDL.ticks
  events <- collectEvents
  keyState <- SDL.getKeyboardState
  update events keyState world >>= \case
    Left Nothing ->
      pure world
    Left (Just err) ->
      putStrLn err >> pure world
    Right newWorld -> do
      render newWorld
      new_tick <- SDL.ticks
      regulateTicks 17 tick new_tick
      if checkEvent SDL.QuitEvent events
      then pure world
      else apploop newWorld update render

setBGColor :: Linear.V4 Word8 -> SDL.Renderer -> IO SDL.Renderer
setBGColor color renderer = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  pure renderer

-- |update window
updateWindow :: SDL.Window -> IO ()
updateWindow = SDL.updateWindowSurface

-- |collect all events from inputs
collectEvents :: IO [SDL.EventPayload]
collectEvents = SDL.pollEvent >>= \case
    Nothing -> return []
    Just e  -> (SDL.eventPayload e :) <$> collectEvents

-- |checks if specific event happend
checkEvent :: SDL.EventPayload -> [SDL.EventPayload] -> Bool
checkEvent = elem

-- |will delay until ticks pass
regulateTicks :: Word.Word32 -> Word.Word32 -> Word.Word32 -> IO ()
regulateTicks ticks tick new_tick =
  when (ticks - (new_tick - tick) < ticks && (ticks - (new_tick - tick)) > 0) $
    SDL.delay $ ticks - (new_tick - tick)
