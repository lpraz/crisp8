module Crisp8.Machine.Keypad where

import Data.Word (Word8)
import Data.Array (Array, listArray)

data KeyState = Up | Down
    deriving Eq

newtype Keypad = Keypad { unKp :: Array Word8 KeyState }

makeKeypad :: Keypad
makeKeypad = Keypad $ listArray (0x0, 0xF) (repeat Up)