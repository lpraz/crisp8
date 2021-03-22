{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import qualified Data.ByteString as B
import Foreign.C.Types (CInt(..))
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.Array.Unboxed as UA
import Control.Concurrent
import System.Environment (getArgs)

import qualified Machine as M

main :: IO ()
main = do
    initializeAll
    window <- createWindow "crisp8" defaultWindow
    windowSize window $= chip8DisplaySize ^* 8
    renderer <- createRenderer window (-1) defaultRenderer
    args <- getArgs
    rom <- B.readFile $ head args
    machine <- M.makeMachine
    machine <- return $ M.loadRom rom machine
    appLoop machine renderer
    destroyWindow window

appLoop :: M.Machine -> Renderer -> IO ()
appLoop machine renderer = do
    threadDelay $ 1000000 `div` 700
    either handleError keepLooping (M.cycle machine)
    where
      handleError msg = do
        print msg
        updateDisplay machine renderer
        exit <- userAskedToExit renderer
        unless exit (appLoop machine renderer)
      keepLooping newMachine = do
        updateDisplay machine renderer
        exit <- userAskedToExit renderer
        unless exit (appLoop newMachine renderer)

updateDisplay :: M.Machine -> Renderer -> IO ()
updateDisplay machine renderer = do
    rendererDrawColor renderer $= offColor
    clear renderer
    texture <- getTexture (M.screen machine) renderer
    copy renderer texture Nothing Nothing
    destroyTexture texture
    present renderer

getTexture :: UA.UArray (Word8, Word8) Bool -> Renderer -> IO Texture
getTexture screen renderer = do
    texture <- createTexture renderer RGB24 TextureAccessStreaming chip8DisplaySize
    let screenBytes = getScreenByteString screen
    updateTexture texture Nothing screenBytes (fromIntegral (M.displayWidth * 3))

getScreenByteString :: UA.UArray (Word8, Word8) Bool -> B.ByteString
getScreenByteString screen = B.pack bytes
    where
      -- We have (x, y), but Haskell's (a, b) Ord gives pixels in column order
      -- instead of row order when we use UA.elems.
      -- Flip coords to order correctly
      flipped = UA.ixmap 
        ((0, 0), (M.displayHeight - 1, M.displayWidth - 1))
        (\(x, y) -> (y, x))
        screen
      bools = UA.elems flipped
      bytes = concatMap (\b -> if b then [255, 255, 255] else [0, 0, 0]) bools

userAskedToExit :: Renderer -> IO Bool
userAskedToExit renderer = do
    events <- pollEvents
    let eventIsQPress event =
          case eventPayload event of
            WindowClosedEvent windowClosedEvent -> True
            _ -> False
    return $ any eventIsQPress events
  
offColor = V4 0 0 0 0
onColor = V4 255 255 255 0

chip8DisplaySize = V2 (fromIntegral M.displayWidth) (fromIntegral M.displayHeight)