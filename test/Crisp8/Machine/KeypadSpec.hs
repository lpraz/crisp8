{-# LANGUAGE StandaloneDeriving #-}

module Crisp8.Machine.KeypadSpec where

import Data.Array
import Data.List
import Data.Word (Word8)

import qualified Crisp8.Machine.Keypad as KP

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec = do
  describe "Crisp8.Machine.Keypad.makeKeypad" $
    it "is a keypad with all keys up and no keypress events" $ do
      let keypad = KP.makeKeypad
      (elems . KP.keys) keypad `shouldSatisfy` all (==KP.Up)
      KP.events keypad `shouldSatisfy` null
  
  describe "Crisp8.Machine.Keypad.updateKeypad" $ do
    prop "should have the new keypress events" (\es ->
      KP.events (KP.updateKeypad es KP.makeKeypad) `shouldBe` es)
    
    prop "should have updated key states based on the events" (\es ->
      -- TODO: values above 16?
      let keypad = KP.updateKeypad es KP.makeKeypad
          keypadKeys = filter
                       (\k -> any (\e -> KP.key e == fst k) es) $
                       (assocs . KP.keys) keypad
          eventKeys = map last . groupBy (\a b -> fst a == fst b) .
                      sortOn fst $
                      map (\e -> (KP.key e, KP.state e)) es
      in eventKeys `shouldBe` keypadKeys)

deriving instance Show KP.KeyState
deriving instance Eq KP.KeyEvent
deriving instance Show KP.KeyEvent
deriving instance Show KP.Keypad

instance Arbitrary KP.KeyState where
    arbitrary = elements [KP.Up, KP.Down]

instance Arbitrary KP.KeyEvent where
    arbitrary = KP.KeyEvent <$> 
                choose (0, 15) <*> 
                arbitrary