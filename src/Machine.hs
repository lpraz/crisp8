module Machine where

import Data.Array.Unboxed as UA
import Data.Word (Word8, Word16)
import Data.Bits (shift, (.&.), (.|.), xor, testBit)
import qualified Data.ByteString as B
import Numeric (showHex)

-- TODO: remove this
import qualified Debug.Trace as DB

-- TODO: compat with SUPER-CHIP, XO-CHIP (use GADT?)
data Machine = Machine
    { ram :: UA.UArray Word16 Word8 -- up to 4kb for original CHIP-8
    , screen :: UArray (Word8, Word8) Bool -- 64x32x1bpp for original CHIP-8
    , pc :: Word16 -- program counter
    , i :: Word16 -- index register
    , stack :: [Word16] -- should this be in CHIP-8 RAM?
    , delayTimer :: Word8
    , soundTimer :: Word8
    , vars :: UA.UArray Word8 Word8 -- variable registers
    }

displayWidth = 64
displayHeight = 32

newtype Instruction = Instruction { execute :: Machine -> Machine }

makeMachine :: Machine
makeMachine = Machine
    { ram = makeRam
    , screen = makeScreen
    , pc = 0x200
    , i = 0
    , stack = []
    , delayTimer = 0
    , soundTimer = 0
    , vars = makeVars
    }

makeRam :: UA.UArray Word16 Word8
makeRam = UA.array (0, 4095) []

makeScreen :: UA.UArray (Word8, Word8) Bool
makeScreen = UA.array ((0, 0), (displayWidth - 1, displayHeight - 1)) []

makeVars :: UA.UArray Word8 Word8
makeVars = UA.array (0, 15) []

load :: B.ByteString -> Machine -> Machine
load bytes machine = machine { ram = newRam }
    where
      byteList = B.unpack bytes
      assocs = zip [0x200..] byteList
      newRam = ram machine // assocs

cycle :: Machine -> Either String Machine
cycle machine = do
    let opcode = fetch machine
    let newMachine = incrementPc machine
    instruction <- decode opcode
    pure $ execute instruction newMachine

fetch :: Machine -> Word16
fetch machine = xxyy byte1 byte2
    where
      mRam = ram machine
      mPc = pc machine
      byte1 = mRam ! mPc
      byte2 = mRam ! (mPc + 1)

incrementPc :: Machine -> Machine
incrementPc machine = machine { pc = pc machine + 2 }

decode :: Word16 -> Either String Instruction
decode word = case word .&. 0xF000 of
    0x0000 -> case word .&. 0xFFF of
        0x0E0 -> Right $ Instruction clearScreen
        0x0EE -> Right $ Instruction returnFromSub
        _ -> Left $ "Unsupported machine language routine " ++ showHex word ""
    0x1000 -> Right $ Instruction $ jump (iNNN word)
    0x2000 -> Right $ Instruction $ callSub (iNNN word)
    0x3000 -> Right $ Instruction $ skipIfEqualTo (iNii word) (iiNN word)
    0x4000 -> Right $ Instruction $ skipIfNotEqualTo (iNii word) (iiNN word)
    0x5000 -> Right $ Instruction $ skipIfEqualToVar (iNii word) (iiNi word)
    0x6000 -> Right $ Instruction $ setVar (iNii word) (iiNN word)
    0x7000 -> Right $ Instruction $ addToVar (iNii word) (iiNN word)
    0x8000 -> case word .&. 0xF of
        0x0 -> Right $ Instruction $ setVarToVar (iNii word) (iiNi word)
        0x1 -> Right $ Instruction $ orWithVars (iNii word) (iiNi word)
        0x2 -> Right $ Instruction $ andWithVars (iNii word) (iiNi word)
        0x3 -> Right $ Instruction $ xorWithVars (iNii word) (iiNi word)
        0x4 -> Right $ Instruction $ addWithVars (iNii word) (iiNi word)
        0x5 -> Right $ Instruction $ subtractFromVar (iNii word) (iiNi word)
        0x6 -> Right $ Instruction $ shiftVarRight (iNii word) (iiNi word)
        0x7 -> Right $ Instruction $ subtractToVar (iNii word) (iiNi word)
        0xE -> Right $ Instruction $ shiftVarLeft (iNii word) (iiNi word)
    0x9000 -> Right $ Instruction $ skipIfNotEqualToVar (iNii word) (iiNi word)
    0xA000 -> Right $ Instruction $ setI (iNNN word)
    0xD000 -> Right $ Instruction $ draw (iNii word) (iiNi word) (iiiN word)
    _ -> Left $ "Invalid opcode " ++ showHex word ""

clearScreen :: Machine -> Machine
clearScreen machine = machine { screen = makeScreen }

returnFromSub :: Machine -> Machine
returnFromSub machine = machine 
    { pc = (head . stack) machine
    , stack = (tail . stack) machine }

jump :: Word16 -> Machine -> Machine
jump newPc machine = machine { pc = newPc }

callSub :: Word16 -> Machine -> Machine
callSub newPc machine = machine
    { pc = newPc
    , stack = pc machine : stack machine
    }

skipIfEqualTo :: Word8 -> Word8 -> Machine -> Machine
skipIfEqualTo xVar value machine = machine { pc = newPc }
    where
      x = vars machine ! xVar
      newPc = if x == value then pc machine + 2 else pc machine

skipIfNotEqualTo :: Word8 -> Word8 -> Machine -> Machine
skipIfNotEqualTo xVar value machine = machine { pc = newPc }
    where
      x = vars machine ! xVar
      newPc = if x /= value then pc machine + 2 else pc machine

skipIfEqualToVar :: Word8 -> Word8 -> Machine -> Machine
skipIfEqualToVar xVar yVar machine = machine { pc = newPc }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newPc = if x == y then pc machine + 2 else pc machine

setVar :: Word8 -> Word8 -> Machine -> Machine
setVar x newVal machine = machine { vars = newVars }
    where newVars = vars machine // [(x, newVal)]

addToVar :: Word8 -> Word8 -> Machine -> Machine
addToVar x addend machine = machine { vars = newVars }
    where newVars = accum (+) (vars machine) [(x, addend)]

setVarToVar :: Word8 -> Word8 -> Machine -> Machine
setVarToVar xVar yVar machine = machine { vars = newVars }
    where
      y = vars machine ! yVar
      newVars = vars machine // [(xVar, y)]

orWithVars :: Word8 -> Word8 -> Machine -> Machine
orWithVars xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine // [(xVar, x .|. y)]

andWithVars :: Word8 -> Word8 -> Machine -> Machine
andWithVars xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine // [(xVar, x .&. y)]

xorWithVars :: Word8 -> Word8 -> Machine -> Machine
xorWithVars xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine // [(xVar, x `xor` y)]

addWithVars :: Word8 -> Word8 -> Machine -> Machine
addWithVars xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, x + y)
        , (0xF, if y > maxBound - x then 1 else 0) -- did we overflow?
        ]

subtractFromVar :: Word8 -> Word8 -> Machine -> Machine
subtractFromVar xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, x - y)
        , (0xF, if x >= y then 1 else 0) -- did we underflow?
        ]

-- Old (COSMAC VIP) version.
-- TODO: make SUPER-CHIP compatible version (as part of SUPER-CHIP compat)
shiftVarRight :: Word8 -> Word8 -> Machine -> Machine
shiftVarRight xVar yVar machine = machine { vars = newVars }
    where
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, shift y (-1))
        , (0xF, if testBit y 0 then 1 else 0) -- what bit got shifted out?
        ]

subtractToVar :: Word8 -> Word8 -> Machine -> Machine
subtractToVar xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, y - x)
        , (0xF, if y >= x then 1 else 0) -- did we underflow?
        ]

-- Old (COSMAC VIP) version.
-- TODO: make SUPER-CHIP compatible version (as part of SUPER-CHIP compat)
shiftVarLeft :: Word8 -> Word8 -> Machine -> Machine
shiftVarLeft xVar yVar machine = machine { vars = newVars }
    where
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, shift y 1)
        , (0xF, if testBit y 7 then 1 else 0) -- what bit got shifted out?
        ]

skipIfNotEqualToVar :: Word8 -> Word8 -> Machine -> Machine
skipIfNotEqualToVar xVar yVar machine = machine { pc = newPc }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newPc = if x /= y then pc machine + 2 else pc machine

setI :: Word16 -> Machine -> Machine
setI newI machine = machine { i = newI }

draw :: Word8 -> Word8 -> Word8 -> Machine -> Machine
draw xVar yVar height machine = machine { screen = newScreen }
    where
      x = (vars machine ! xVar) `mod` displayWidth
      y = (vars machine ! yVar) `mod` displayHeight
      sprite = readSprite (ram machine) (i machine) height
      newScreen = blit (screen machine) sprite x y

readSprite 
    :: UA.UArray Word16 Word8 
    -> Word16 
    -> Word8 
    -> UA.UArray (Word8, Word8) Bool
readSprite ram i height = bits
    where
      bytes16 = ixmap (0, fromIntegral (height - 1)) (+i) ram
      bytes = ixmap (0, height - 1) fromIntegral bytes16
        :: UA.UArray Word8 Word8
      -- Each byte is an 8px-wide row, so our sprite should be 8*height
      bitAssocs = [ ((x, i), b .&. (2^(7 - x)) /= 0)
                    | (i, b) <- assocs bytes, x <- [0..7] ]
      bits = array ((0, 0), (7, height - 1)) bitAssocs

blit
    :: UA.UArray (Word8, Word8) Bool
    -> UA.UArray (Word8, Word8) Bool
    -> Word8
    -> Word8
    -> UA.UArray (Word8, Word8) Bool
blit screen sprite x y = accum
    (/=)
    screen
    [ ((ax + x, ay + y), s) | ((ax, ay), s) <- assocs sprite ]

iNNN :: Word16 -> Word16
iNNN = (0xFFF .&.)

iNii :: Word16 -> Word8
iNii x = fromIntegral $ shift (0xF00 .&. x) (-8)

iiNN :: Word16 -> Word8
iiNN = fromIntegral . (0xFF .&.)

iiNi :: Word16 -> Word8
iiNi x = fromIntegral $ shift (0xF0 .&. x) (-4)

iiiN :: Word16 -> Word8
iiiN = fromIntegral . (0xF .&.)

xxyy :: Word8 -> Word8 -> Word16
xxyy x y = shift x16 8 + y16
    where
      x16 = fromIntegral x :: Word16
      y16 = fromIntegral y :: Word16