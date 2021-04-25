{-# LANGUAGE OverloadedStrings #-}
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
    initializeAll
    let windowConfig = defaultWindow 
          { windowInitialSize = G.chip8DisplaySize ^* 8 
          }
    window <- createWindow "crisp8" windowConfig
    renderer <- createRenderer window (-1) defaultRenderer
    rom <- fmap head getArgs >>= B.readFile
    machine <- fmap (M.loadRom rom) M.makeMachine
    appLoop machine renderer
    destroyWindow window

-- Multithreadedness:
-- forkIO $ someFunc to split into machine, display and timer threads

appLoop :: M.Machine -> Renderer -> IO ()
appLoop machine renderer = do
    threadDelay $ 1_000_000 `div` M.clockSpeed machine
    either print keepLooping (M.cycle machine)
    where
      keepLooping cycledMachine = do
        G.updateDisplay cycledMachine renderer
        newKeypad <- I.updateKeys $ M.keypad cycledMachine
        let kpUpdatedMachine = cycledMachine { M.keypad = newKeypad }
        exit <- userAskedToExit
        unless exit $ appLoop kpUpdatedMachine renderer

userAskedToExit :: IO Bool
userAskedToExit = do
    events <- pollEvents
    let eventIsWindowClose event =
          case eventPayload event of
            WindowClosedEvent windowClosedEvent -> True
            _ -> False
    return $ any eventIsWindowClose events