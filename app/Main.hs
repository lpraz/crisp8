{-# LANGUAGE NumericUnderscores #-}

module Main where

import SDL
import Foreign.C.Types (CInt(..))
import Control.Monad (unless)
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import System.Environment (getArgs)

import qualified Crisp8.Machine as M
import qualified Crisp8.Machine.Keypad as KP
import qualified Crisp8.Ui.Input as I
import qualified Crisp8.Ui.Graphics as G

main :: IO ()
main = do
    window <- G.makeWindow
    renderer <- createRenderer window (-1) defaultRenderer
    rom <- fmap head getArgs >>= B.readFile
    machine <- fmap (M.loadRom rom) M.makeMachine
    mMachine <- newMVar machine
    forkIO $ displayLoop renderer mMachine
    forkIO $ timerLoop mMachine
    machineLoop mMachine
    destroyWindow window

-- Machine: run every 1s/clockSpeed, cycles machine and handles input
machineLoop :: MVar M.Machine -> IO ()
machineLoop mMachine = do
    modifyMVar_ mMachine (\machine -> do
      threadDelay $ 1_000_000 `div` M.clockSpeed machine
      newKeypad <- I.updateKeys $ M.keypad machine
      let kpUpdatedMachine = machine { M.keypad = newKeypad }
      either (\e -> print e >> pure machine) pure $ M.cycle kpUpdatedMachine)
    exit <- I.userAskedToExit
    unless exit $ machineLoop mMachine

-- Display: 60Hz (configurable?), updates window based on machine display
displayLoop :: Renderer -> MVar M.Machine -> IO ()
displayLoop renderer mMachine = do
    withMVar mMachine $ G.updateDisplay renderer
    threadDelay $ 1_000_000 `div` 60
    displayLoop renderer mMachine

-- Timer: 60Hz, decrements delay/sound timers
timerLoop :: MVar M.Machine -> IO ()
timerLoop mMachine = do
    modifyMVar_ mMachine (pure . M.decrementTimers)
    threadDelay $ 1_000_000 `div` 60
    timerLoop mMachine