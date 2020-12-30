module Machine where

import Data.Array.Unboxed as UA
import Data.Word (Word8, Word16)
import qualified Timer as T
import qualified ProgramCounter as PC

-- TODO: compat with SuperCHIP-8, XO-CHIP (use GADT?)
data Machine = Machine
    { ram :: UA.UArray Word16 Word8 -- up to 4kb for original CHIP-8
    , screen :: UArray (Word8, Word8) Bool -- 64x32x1bpp for original CHIP-8
    , pc :: PC.ProgramCounter
    , i :: Word16
    , stack :: [Word16] -- should this be in CHIP-8 RAM?
    , delayTimer :: T.Timer
    , soundTimer :: T.Timer
    , vars :: UA.UArray Word Word8
    }

makeMachine :: Machine
makeMachine = Machine
    { ram = makeRam
    , screen = makeScreen
    , pc = PC.makePc
    , i = 0
    , stack = []
    , delayTimer = T.set 0
    , soundTimer = T.set 0
    , vars = makeVars
    }

makeRam :: UA.UArray Word16 Word8
makeRam = UA.array (0, 4095) []

makeScreen :: UA.UArray (Word8, Word8) Bool
makeScreen = UA.array ((0, 0), (64, 32)) []

makeVars :: UA.UArray Word Word8
makeVars = UA.array (0, 15) []