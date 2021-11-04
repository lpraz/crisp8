{-# LANGUAGE StandaloneDeriving #-}

module Crisp8.MachineSpec where

import Data.Array.Unboxed
import Data.ByteString (pack)
import Data.Word
import Data.Bits(shift, (.&.))

import qualified Crisp8.Machine as M
import qualified Crisp8.Machine.Keypad as KP

import Test.Hspec
import Test.Hspec.QuickCheck

spec = do
  describe "Crisp8.Machine.makeMachine" $ do
    it "inits the CHIP-8 machine with 4K RAM" $ do
      machine <- M.makeMachine
      (bounds . M.ram) machine `shouldBe` (0, 0xFFF)
    
    it "inits the CHIP-8 RAM empty above the first 512 bytes" $ do
      machine <- M.makeMachine
      (drop 0x200 . elems . M.ram) machine `shouldSatisfy` all (==0)
    
    it "inits the CHIP-8 machine with a 64x32 screen" $ do
      machine <- M.makeMachine
      (bounds . M.screen) machine `shouldBe` ((0, 0), (63, 31))
      
    it "inits the CHIP-8 screen with all 'off' pixels" $ do
      machine <- M.makeMachine
      (elems . M.screen) machine `shouldSatisfy` all (==False)
    
    it "inits the CHIP-8 program counter to 0x200" $ do
      machine <- M.makeMachine
      M.pc machine `shouldBe` 0x200
    
    it "inits the CHIP-8 index register to 0" $ do
      machine <- M.makeMachine
      M.i machine `shouldBe` 0
    
    it "inits the CHIP-8 machine with an empty stack" $ do
      machine <- M.makeMachine
      M.stack machine `shouldSatisfy` null
    
    it "inits the CHIP-8 machine with a zero delay timer" $ do
      machine <- M.makeMachine
      M.delayTimer machine `shouldBe` 0
    
    it "inits the CHIP-8 machine with a zero sound timer" $ do
      machine <- M.makeMachine
      M.soundTimer machine `shouldBe` 0
    
    it "inits the CHIP-8 machine with variable registers 0 through 15" $ do
      machine <- M.makeMachine
      (bounds . M.vars) machine `shouldBe` (0, 15)

    it "inits the CHIP-8 variable registers to all zeroes" $ do
      machine <- M.makeMachine
      (elems . M.vars) machine `shouldSatisfy` all (==0)
    
    it "inits the CHIP-8 machine in the running state" $ do
      machine <- M.makeMachine
      M.runState machine `shouldBe` M.Running
    
    it "inits the CHIP-8 machine with the correct clock speed" $ do
      machine <- M.makeMachine
      M.clockSpeed machine `shouldBe` 700

  describe "Crisp8.Machine.loadRom" $
    prop "loads the ROM into RAM after the first 512 bytes" (\rom -> do
      machine <- M.makeMachine
      let bytes = pack rom
      let loadedMachine = M.loadRom bytes machine
      let contents = take (length rom) . drop 0x200 . elems . M.ram 
      contents loadedMachine `shouldBe` rom)
  
  describe "Crisp8.Machine.cycle" $ do
    it "checks for input when waiting for key up and no key is released" $ do
      machine <- M.makeMachine
      let waitMachine = machine
            { M.runState = M.WaitForKeyUp 0 
            , M.keypad = KP.makeKeypad
            }
      let (Right newMachine) = M.cycle waitMachine
      M.pc newMachine `shouldBe` M.pc waitMachine
      M.runState newMachine `shouldBe` M.WaitForKeyUp 0
    
    it "starts running when waiting for key up and a key is released" $ do
      machine <- M.makeMachine
      let waitMachine = machine
            { M.runState = M.WaitForKeyUp 0
            , M.keypad = KP.makeKeypad { KP.events = [KP.KeyEvent 0 KP.Up] }
            }
      let (Right newMachine) = M.cycle waitMachine
      M.pc newMachine `shouldBe` M.pc waitMachine
      M.runState newMachine `shouldBe` M.Running
    
    it "does nothing when paused" $ do
      machine <- M.makeMachine
      let pausedMachine = machine { M.runState = M.Paused }
      let (Right newMachine) = M.cycle pausedMachine
      M.pc machine `shouldBe` M.pc newMachine
    
    it "increments the program counter when running" $ do
      oldMachine <- fmap (giveInstruction 0x00E0) M.makeMachine 
      let oldPc = M.pc oldMachine
      let (Right newMachine) = M.cycle oldMachine
      let newPc = M.pc newMachine
      newPc `shouldBe` oldPc + 2

deriving instance Show M.RunState
deriving instance Eq M.RunState

giveInstruction :: Word16 -> M.Machine -> M.Machine
giveInstruction inst machine = machine { M.ram = ram }
    where
      upper = (fromIntegral . (`shift` 8)) inst
      lower = (fromIntegral . (0xFF .&.)) inst
      pc = M.pc machine
      ram = M.ram machine // [(pc, upper), (pc + 1, lower)]