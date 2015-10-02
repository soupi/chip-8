{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime.Run (main, runGame) where

import           Data.Maybe (fromMaybe)
import qualified Data.Char as Char (toLower)
import qualified System.Random as Rand
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad          ((>=>), (<=<), when)
import qualified Data.ByteString as BS
import qualified SDL
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Lens.Micro.Mtl as Lens
import qualified Lens.Micro     as Lens
import qualified Options.Applicative as Opt
import Options.Applicative ((<>))

import qualified MySDL.MySDL as MySDL
import qualified CPU.Bits as Bits
import qualified CPU.Disassembler as DA
import qualified CPU.CPU as CPU
import           CPU.CPU   (CPU)
import           CPU.Emulate
import           CPU.Utils (mapLeft, map2, replicateMChain)


-----------
-- Types --
-----------

type World = (Settings, CPU)

data Settings =
  Settings
    { setShowInstructions :: Bool
    , setPlaySounds       :: Bool
    , setSpeed            :: Speed
    }

data Speed = SlowSpeed | NormalSpeed | FastSpeed

defaultSettings :: Settings
defaultSettings =
  Settings
    { setShowInstructions = False
    , setPlaySounds       = False
    , setSpeed            = NormalSpeed
    }

speed2InstPerFrame :: Speed -> Int
speed2InstPerFrame speed =
  case speed of
    SlowSpeed   -> 2
    NormalSpeed -> 10
    FastSpeed   -> 20

-------------------
-- Option Parser --
-------------------

argsParser :: Opt.Parser (FilePath, Settings)
argsParser =
  (,) <$> filepathParser
      <*> settingsParser

filepathParser :: Opt.Parser FilePath
filepathParser =
  Opt.argument Opt.str $
       Opt.metavar "FILE"
    <> Opt.help "Path to the game to load"


settingsParser :: Opt.Parser Settings
settingsParser =
  Settings <$>
      Opt.switch
        (  Opt.long  "instructions"
        <> Opt.short 'i'
        <> Opt.help  "Trace encountered instructions")

     <*> Opt.switch
        (  Opt.long  "no-sound"
        <> Opt.short 'n'
        <> Opt.help  "Run game without sound")

     <*>
       fmap (fromMaybe (setSpeed defaultSettings))
       (Opt.optional
        (Opt.option
          (Opt.str >>= parseSpeed)
          (  Opt.long "speed"
          <> Opt.short 's'
          <> Opt.metavar "SPEED"
          <> Opt.help "Set emulation speed to (slow/normal/fast)")))

parseSpeed :: String -> Opt.ReadM Speed
parseSpeed str =
  case map Char.toLower str of
    "slow"   -> pure SlowSpeed
    "normal" -> pure NormalSpeed
    "fast"   -> pure FastSpeed
    _        -> Opt.readerError $ "'" ++ str ++ "' is not a valid option"

argsParserInfo :: Opt.ParserInfo (FilePath, Settings)
argsParserInfo =
  Opt.info (Opt.helper <*> argsParser) $
     Opt.fullDesc
  <> Opt.progDesc "A purely functional CHIP-8 emulator written in Haskell"
  <> Opt.header   "HIP-8"

-----------
-- Logic --
-----------

main :: IO ()
main = do
  (file, settings) <- Opt.execParser argsParserInfo
  gameContent <- BS.readFile file
  runGame settings gameContent


runGame :: Settings -> BS.ByteString -> IO ()
runGame settings gameData =
  case loadGameAndFonts (Rand.mkStdGen 100) gameData of
    Left (_, err) ->
      putStrLn $ "Could not load game.\nError: " ++ err
    Right result -> do
      putStrLn "Hello CHIP-8!"
      _ <- run (settings, result)
      putStrLn "Goodbye."

run :: World -> IO World
run world =
  MySDL.withWindow "CHIP-8" (MySDL.myWindowConfig (Linear.V2 512 256)) $
    flip MySDL.withRenderer (setBGColor >=> MySDL.apploop world update . render)

update :: MonadIO m => [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> World -> m (Either (Maybe String) World)
update _ keysState (settings, cpu) =
  pure . fmap ((,) settings) . mapLeft (pure . CPU.showErr) . (pure . updateTimers <=< replicateMChain times emulateCycle) . setKeys keysState . CPU.clearKeys $ cpu
    where times = speed2InstPerFrame (setSpeed settings)


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


render :: MonadIO m => (SDL.Window, SDL.Renderer) -> World -> m ()
render (_, renderer) (settings, cpu) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  drawRects (Lens.view CPU.gfx cpu) renderer
  SDL.present renderer
  when (Lens.view CPU.soundTimer cpu > 0) $ pure ()
  when (setShowInstructions settings) $
    case fetch cpu of -- show instructions for debug purposes
      Left _   -> pure ()
      Right op -> do
        liftIO $ putStr (Bits.showHex16 $ fromIntegral (CPU.getPC cpu))
        liftIO $ putStrLn $ ": " ++ DA.showOpcode op


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

