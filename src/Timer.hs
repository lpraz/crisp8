module Timer (Timer, set, decrement) where

import Data.Word (Word8)

newtype Timer = Timer Word8

set :: Word8  -> Timer
set = Timer

decrement :: Timer -> Timer
decrement (Timer 0) = Timer 0
decrement (Timer n) = Timer (n - 1)