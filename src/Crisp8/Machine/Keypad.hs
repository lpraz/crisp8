module Crisp8.Machine.Keypad where

import Data.Word (Word8)
import Data.Array (Array, listArray, (//))

data KeyState = Up | Down
    deriving Eq

data KeyEvent = KeyEvent
    { key :: Word8
    , state :: KeyState
    }

data Keypad = Keypad
    { keys :: Array Word8 KeyState
    , events :: [KeyEvent]
    }

makeKeypad :: Keypad
makeKeypad = Keypad
    { keys = listArray (0x0, 0xF) (repeat Up)
    , events = []
    }

updateKeypad :: [KeyEvent] -> Keypad -> Keypad
updateKeypad events kp = kp { keys = newKeys, events = events }
    where newKeys = keys kp // map (\ke -> (key ke, state ke)) events