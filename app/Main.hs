{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import qualified Data.ByteString as B
import Foreign.C.Types (CInt(..))
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.Array.Unboxed as UA
import Control.Concurrent
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)

import qualified Machine as M
import qualified Keypad as KP

main :: IO ()
main = do
    initializeAll
    window <- createWindow "crisp8" defaultWindow
    windowSize window $= chip8DisplaySize ^* 8
    renderer <- createRenderer window (-1) defaultRenderer
    args <- getArgs
    rom <- B.readFile $ head args
    machine <- M.makeMachine
    machine <- return $ M.loadRom rom machine
    appLoop machine renderer
    destroyWindow window

appLoop :: M.Machine -> Renderer -> IO ()
appLoop machine renderer = do
    threadDelay $ 1_000_000 `div` M.clockSpeed machine
    either print keepLooping (M.cycle machine)
    where
      keepLooping cycledMachine = do
        updateDisplay cycledMachine renderer
        newKeypad <- updateKeys $ M.keypad cycledMachine
        let kpUpdatedMachine = cycledMachine { M.keypad = newKeypad }
        exit <- userAskedToExit renderer
        unless exit (appLoop kpUpdatedMachine renderer)

updateDisplay :: M.Machine -> Renderer -> IO ()
updateDisplay machine renderer = do
    rendererDrawColor renderer $= offColor
    clear renderer
    texture <- getTexture (M.screen machine) renderer
    copy renderer texture Nothing Nothing
    destroyTexture texture
    present renderer

updateKeys :: KP.Keypad -> IO KP.Keypad
updateKeys keypad = do
    events <- pollEvents
    let kbEventMap event = 
          case eventPayload event of
            KeyboardEvent kbEvent -> Just kbEvent
            _ -> Nothing
    let kbEvents = mapMaybe kbEventMap events
    let oldKpArray = KP.unKp keypad
    let newKpAssocs = mapMaybe getChip8KeyEvent kbEvents
    let newKpArray = oldKpArray UA.// newKpAssocs
    return $ KP.Keypad newKpArray

getChip8KeyEvent :: KeyboardEventData -> Maybe (Word8, KP.KeyState)
getChip8KeyEvent kbEvent = do
  chip8Key <- getChip8Key $ keysymScancode $ keyboardEventKeysym kbEvent
  let chip8KeyState = getChip8KeyState $ keyboardEventKeyMotion kbEvent
  return (chip8Key, chip8KeyState)

-- Follows COSMAC VIP layout.
-- TODO: allow config for this, with presets that follow other systems?
getChip8Key :: Scancode -> Maybe Word8
getChip8Key Scancode1 = Just 0x0
getChip8Key Scancode2 = Just 0x1
getChip8Key Scancode3 = Just 0x2
getChip8Key Scancode4 = Just 0xC
getChip8Key ScancodeQ = Just 0x4
getChip8Key ScancodeW = Just 0x5
getChip8Key ScancodeE = Just 0x6
getChip8Key ScancodeR = Just 0xD
getChip8Key ScancodeA = Just 0x7
getChip8Key ScancodeS = Just 0x8
getChip8Key ScancodeD = Just 0x9
getChip8Key ScancodeF = Just 0xE
getChip8Key ScancodeZ = Just 0xA
getChip8Key ScancodeX = Just 0x0
getChip8Key ScancodeC = Just 0xB
getChip8Key ScancodeV = Just 0xF
getChip8Key _ = Nothing

getChip8KeyState :: InputMotion -> KP.KeyState
getChip8KeyState Pressed = KP.Down
getChip8KeyState Released = KP.Up

getTexture :: UA.UArray (Word8, Word8) Bool -> Renderer -> IO Texture
getTexture screen renderer = do
    texture <- createTexture renderer RGB24 TextureAccessStreaming chip8DisplaySize
    let screenBytes = getScreenByteString screen
    updateTexture texture Nothing screenBytes (fromIntegral (M.displayWidth * 3))

getScreenByteString :: UA.UArray (Word8, Word8) Bool -> B.ByteString
getScreenByteString screen = B.pack bytes
    where
      -- We have (x, y), but Haskell's (a, b) Ord gives pixels in column order
      -- instead of row order when we use UA.elems.
      -- Flip coords to order correctly
      flipped = UA.ixmap 
        ((0, 0), (M.displayHeight - 1, M.displayWidth - 1))
        (\(x, y) -> (y, x))
        screen
      bools = UA.elems flipped
      bytes = concatMap (\b -> if b then [255, 255, 255] else [0, 0, 0]) bools

userAskedToExit :: Renderer -> IO Bool
userAskedToExit renderer = do
    events <- pollEvents
    let eventIsWindowClose event =
          case eventPayload event of
            WindowClosedEvent windowClosedEvent -> True
            _ -> False
    return $ any eventIsWindowClose events
  
offColor = V4 0 0 0 0
onColor = V4 255 255 255 0

chip8DisplaySize = V2 (fromIntegral M.displayWidth) (fromIntegral M.displayHeight)