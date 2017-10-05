{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Runtime.Run (main, runGame) where

import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Char as Char (toLower)
import qualified System.Random as Rand
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad          ((>=>), (<=<), when, void, forever)
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified SDL
import qualified Sound.Tomato.Speakers as Tomato
import qualified Linear
import qualified Linear.Affine as Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Foreign.C.Types as C (CInt)
import qualified Lens.Micro.Mtl as Lens
import qualified Lens.Micro     as Lens
import qualified Options.Applicative as Opt



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

type World = (Settings, Chan Word8, CPU)

data Settings = Settings
  { setShowInstructions :: Bool
  , setPlaySounds       :: Bool
  , setSpeed            :: Speed
  }

data Speed
  = SlowSpeed
  | NormalSpeed
  | FastSpeed

defaultSettings :: Settings
defaultSettings = Settings
  { setShowInstructions = False
  , setPlaySounds       = True
  , setSpeed            = NormalSpeed
  }

speed2InstPerFrame :: Speed -> Int
speed2InstPerFrame speed =
  case speed of
    SlowSpeed   -> 1
    NormalSpeed -> 2
    FastSpeed   -> 5

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

     <*> fmap not (Opt.switch
        (  Opt.long  "no-sound"
        <> Opt.short 'n'
        <> Opt.help  "Run game without sound"))

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
      audioChan <- newChan
      forkIO (audioHandler (setSpeed settings) audioChan)
      _ <- run (settings, audioChan, result)
      putStrLn "Goodbye."

run :: World -> IO World
run world =
  MySDL.withWindow "CHIP-8" (MySDL.myWindowConfig (Linear.V2 512 256)) $
    flip MySDL.withRenderer (setBGColor >=> MySDL.apploop world update . render)

update :: [SDL.EventPayload] -> (SDL.Scancode -> Bool) -> World -> IO (Either (Maybe String) World)
update _ keysState (settings, audioChan, cpu) =
  pure
  . fmap ((,,) settings audioChan)
  . mapLeft (pure . CPU.showErr)
  . (pure . updateTimers <=< replicateMChain times emulateCycle . cleanSoundTimer)
  . setKeys keysState
  . CPU.clearKeys
  $ cpu
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


setBGColor :: (SDL.Window, SDL.Renderer) -> IO (SDL.Window, SDL.Renderer)
setBGColor sdlStuff@(_, renderer) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  pure sdlStuff


render :: (SDL.Window, SDL.Renderer) -> World -> IO ()
render (_, renderer) (settings, audioChan, cpu) = do
  MySDL.setBGColor (Linear.V4 0 0 0 255) renderer
  drawRects (Lens.view CPU.gfx cpu) renderer
  SDL.present renderer
  when (setPlaySounds settings && Lens.view CPU.soundTimer cpu > 0) $
    writeChan audioChan (Lens.view CPU.soundTimer cpu)
  when (setShowInstructions settings) $
    case fetch cpu of -- show instructions for debug purposes
      Left _   -> pure ()
      Right op -> do
        putStr (Bits.showHex16 $ fromIntegral (CPU.getPC cpu))
        putStrLn $ ": " ++ DA.showOpcode op


squareSize :: C.CInt
squareSize =  8

convertGFX :: V.Vector Bool -> V.Vector (SDL.Rectangle C.CInt)
convertGFX gfx = V.map f $ V.filter snd $ V.indexed gfx
    where f (index, _)  = SDL.Rectangle (Linear.P $ uncurry Linear.V2 (determinePos $ fromIntegral index)) (Linear.V2 squareSize squareSize)
          determinePos i = map2 (* squareSize) (i `mod` 64, i `div` 64)

drawRects :: V.Vector Bool -> SDL.Renderer -> IO ()
drawRects gfx renderer = do
  SDL.rendererDrawColor renderer SDL.$= Linear.V4 255 255 255 255
  let newGfx = convertGFX gfx
      rects  = VS.generate (V.length newGfx) (newGfx V.!)
  SDL.drawRects renderer rects
  SDL.fillRects renderer rects


-- taken from here: http://hackage.haskell.org/package/tomato-rubato-openal-0.1.0.4/docs/src/Sound-Tomato-Speakers.html#testSine
beep :: Float -> IO ()
beep duration = void $ Tomato.withSpeakers sampleRate 128 $ flip Tomato.playSamples sound
  where
    freq        = 1000
    sampleRate  = 22050
    dt          = 1 / sampleRate -- time in seconds of a single sample
    sound       = take (ceiling $ duration / dt)
      $ map (0.4*) sine

    sine = [sin (2*pi*freq*dt*fromIntegral t) | t <- [0..] :: [Integer]]


audioHandler :: Speed -> Chan Word8 -> IO ()
audioHandler spd chan = forever $ do
  duration <- readChan chan
  beep (fromIntegral duration / speedToDivider spd)

speedToDivider :: Speed -> Float
speedToDivider = (*10) . fromIntegral . speed2InstPerFrame

