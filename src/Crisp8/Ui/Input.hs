module Crisp8.Ui.Input where

import SDL
import Data.Array.Unboxed
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import qualified Crisp8.Machine.Keypad as KP

updateKeys :: [Event] -> KP.Keypad -> KP.Keypad
updateKeys events = KP.updateKeypad kpEvents
    where
      kbEventMap event = 
          case eventPayload event of
            KeyboardEvent kbEvent -> Just kbEvent
            _ -> Nothing
      kbEvents = mapMaybe kbEventMap events
      kpEvents = mapMaybe getChip8KeyEvent kbEvents

getChip8KeyEvent :: KeyboardEventData -> Maybe KP.KeyEvent
getChip8KeyEvent kbEvent = do
  chip8Key <- getChip8Key $ keysymScancode $ keyboardEventKeysym kbEvent
  let chip8KeyState = getChip8KeyState $ keyboardEventKeyMotion kbEvent
  pure $ KP.KeyEvent chip8Key chip8KeyState

-- Follows COSMAC VIP layout.
-- TODO: allow config for this, with presets that follow other systems?
getChip8Key :: Scancode -> Maybe Word8
getChip8Key Scancode1 = Just 0x1
getChip8Key Scancode2 = Just 0x2
getChip8Key Scancode3 = Just 0x3
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

userAskedToExit :: [Event] -> Bool
userAskedToExit = any eventIsWindowClose
    where eventIsWindowClose event =
            case eventPayload event of
              WindowClosedEvent windowClosedEvent -> True
              _ -> False