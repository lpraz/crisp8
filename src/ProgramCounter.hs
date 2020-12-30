module ProgramCounter (ProgramCounter, makePc, increment) where

import Data.Word (Word16)

newtype ProgramCounter = ProgramCounter Word16

makePc :: ProgramCounter
makePc = ProgramCounter 0

increment :: ProgramCounter -> ProgramCounter
increment (ProgramCounter n) = ProgramCounter (n + 2)

set :: Word16 -> ProgramCounter -> ProgramCounter
set n (ProgramCounter _) = ProgramCounter n