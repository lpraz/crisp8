module Machine where

import Data.Array.Unboxed as UA
import Data.Word (Word8, Word16)
import Data.Bits (shift, (.&.), (.|.), xor, testBit)
import qualified Data.ByteString as B
import Numeric (showHex)
import System.Random (StdGen, newStdGen, random)

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
    , rng :: StdGen
    }

displayWidth = 64
displayHeight = 32

newtype Instruction = Instruction { execute :: Machine -> Machine }

makeMachine :: IO Machine
makeMachine = do
    rng <- newStdGen
    return Machine
      { ram = makeRam
      , screen = makeScreen
      , pc = 0x200
      , i = 0
      , stack = []
      , delayTimer = 0
      , soundTimer = 0
      , vars = makeVars
      , rng = rng
      }

makeRam :: UA.UArray Word16 Word8
makeRam = UA.array (0, 4095) []

makeScreen :: UA.UArray (Word8, Word8) Bool
makeScreen = UA.array ((0, 0), (displayWidth - 1, displayHeight - 1)) []

makeVars :: UA.UArray Word8 Word8
makeVars = UA.array (0, 15) []

loadRom :: B.ByteString -> Machine -> Machine
loadRom bytes machine = machine { ram = newRam }
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
        0x0E0 -> inst clearScreen
        0x0EE -> inst returnFromSub
        _ -> Left $ "Unsupported machine language routine " ++ showHex word ""
    0x1000 -> inst $ jump (iNNN word)
    0x2000 -> inst $ callSub (iNNN word)
    0x3000 -> inst $ skipIfEqualTo (iNii word) (iiNN word)
    0x4000 -> inst $ skipIfNotEqualTo (iNii word) (iiNN word)
    0x5000 -> inst $ skipIfEqualToVar (iNii word) (iiNi word)
    0x6000 -> inst $ setVar (iNii word) (iiNN word)
    0x7000 -> inst $ addToVar (iNii word) (iiNN word)
    0x8000 -> case word .&. 0xF of
        0x0 -> inst $ setVarToVar (iNii word) (iiNi word)
        0x1 -> inst $ orWithVars (iNii word) (iiNi word)
        0x2 -> inst $ andWithVars (iNii word) (iiNi word)
        0x3 -> inst $ xorWithVars (iNii word) (iiNi word)
        0x4 -> inst $ addWithVars (iNii word) (iiNi word)
        0x5 -> inst $ subtractFromVar (iNii word) (iiNi word)
        0x6 -> inst $ shiftVarRight (iNii word) (iiNi word)
        0x7 -> inst $ subtractToVar (iNii word) (iiNi word)
        0xE -> inst $ shiftVarLeft (iNii word) (iiNi word)
        _ -> invalid
    0x9000 -> inst $ skipIfNotEqualToVar (iNii word) (iiNi word)
    0xA000 -> inst $ setI (iNNN word)
    0xB000 -> inst $ jumpWithOffset (iNNN word)
    0xC000 -> inst $ setVarToMaskedRandom (iNii word) (iiNN word)
    0xD000 -> inst $ draw (iNii word) (iiNi word) (iiiN word)
    0xF000 -> case word .&. 0xFF of
        --0x07 -> inst $ readDelayTimer (iNii word)
        --0x15 -> inst $ setDelayTimer (iNii word)
        --0x18 -> inst $ setSoundTimer (iNii word)
        0x1E -> inst $ addToI (iNii word)
        0x33 -> inst $ convertBcd (iNii word)
        0x55 -> inst $ store (iNii word)
        0x65 -> inst $ load (iNii word)
        _ -> invalid
    _ -> invalid
    where
      inst x = Right $ Instruction x
      invalid = Left $ "Invalid opcode " ++ showHex word ""

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
        , (0xF, if willOverflowOnAdd x y maxBound then 1 else 0)
        ]

subtractFromVar :: Word8 -> Word8 -> Machine -> Machine
subtractFromVar xVar yVar machine = machine { vars = newVars }
    where
      x = vars machine ! xVar
      y = vars machine ! yVar
      newVars = vars machine //
        [ (xVar, x - y)
        , (0xF, if willUnderflowOnSubtract x y then 0 else 1)
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
        , (0xF, if willUnderflowOnSubtract y x then 0 else 1)
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

-- Old (COSMAC VIP) version. Always uses V0 as offset
-- TODO: make SUPER-CHIP compatible version (as part of SUPER-CHIP compat)
jumpWithOffset :: Word16 -> Machine -> Machine
jumpWithOffset newPc machine = machine { pc = offsetPc }
    where
      offset = vars machine ! 0
      offsetPc = newPc + fromIntegral offset

setVarToMaskedRandom :: Word8 -> Word8 -> Machine -> Machine
setVarToMaskedRandom xVar mask machine = machine
    { vars = newVars
    , rng = newRng 
    }
    where
      (randomWord, newRng) = (random . rng) machine
      newVars = vars machine // [(xVar, randomWord)]

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

--readDelayTimer :: Word8 -> Machine -> Machine
--readDelayTimer xVar machine = machine { vars = newVars }
--    where
--      newVars = vars machine // [(xVar, delayTimer machine)]

--setDelayTimer :: Word8 -> Machine -> Machine
--setDelayTimer xVar machine = machine { delayTimer = newTimer }
--    where
--      newTimer = vars machine ! xVar

--setSoundTimer :: Word8 -> Machine -> Machine
--setSoundTimer xVar machine = machine { soundTimer = newTimer }
--    where
--      newTimer = vars machine ! xVar

-- Sets VF based on overflow, in line with Amiga
addToI :: Word8 -> Machine -> Machine
addToI xVar machine = machine { i = newI, vars = newVars }
    where
      x = vars machine ! xVar
      newI = i machine + fromIntegral x
      newVars = vars machine //
        [(0xF, if willOverflowOnAdd (fromIntegral x) (i machine) 0xFFF then 1 else 0)]

convertBcd :: Word8 -> Machine -> Machine
convertBcd xVar machine = machine { ram = newRam }
    where
      x = vars machine ! xVar
      hundreds = x `div` 100
      tens = x `div` 10 `mod` 10
      ones = x `mod` 10
      newRam = ram machine //
        [ (i machine, hundreds)
        , (i machine + 1, tens)
        , (i machine + 2, ones)
        ]

-- Old (COSMAC VIP) version. Sets i at end (SUPER-CHIP doesn't)
-- TODO: make SUPER-CHIP compatible version (as part of SUPER-CHIP compat)
store :: Word8 -> Machine -> Machine
store xVar machine = machine { ram = newRam, i = newI }
    where
      vars16 = ixmap (0, 15) fromIntegral (vars machine)
        :: UA.UArray Word16 Word8
      contents = ixmap 
        (i machine, i machine + fromIntegral xVar)
        (\a -> a - i machine)
        vars16
      newRam = ram machine // assocs contents
      newI = i machine + fromIntegral xVar

-- Old (COSMAC VIP) version. Sets i at end (SUPER-CHIP doesn't)
-- TODO: make SUPER-CHIP compatible version (as part of SUPER-CHIP compat)
load :: Word8 -> Machine -> Machine
load xVar machine = machine { vars = newVars, i = newI }
    where
      contents = ixmap
        (0, xVar) 
        (\a -> fromIntegral (fromIntegral a + i machine))
        (ram machine)
      newVars = vars machine // assocs contents
      newI = i machine + fromIntegral xVar

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

willOverflowOnAdd :: (Integral a) => a -> a -> a -> Bool
willOverflowOnAdd x y upperBound = y > upperBound - x

willUnderflowOnSubtract :: (Integral a) => a -> a -> Bool
willUnderflowOnSubtract x y = x <= y