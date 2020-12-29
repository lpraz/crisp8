{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import qualified Data.ByteString as B
import Foreign.C.Types (CInt(..))
import Data.Int (Int32)

main :: IO ()
main = do
    initializeAll
    window <- createWindow "crisp8" defaultWindow
    windowSize window $= chip8DisplaySize ^* 8
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer
    destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
    exit <- wasQPressed renderer
    updateDisplay renderer
    unless exit (appLoop renderer)

updateDisplay :: Renderer -> IO ()
updateDisplay renderer = do
    rendererDrawColor renderer $= offColor
    clear renderer
    texture <- dummyTexture renderer
    copy renderer texture Nothing Nothing
    destroyTexture texture
    present renderer

wasQPressed :: Renderer -> IO Bool
wasQPressed renderer = do
    events <- pollEvents
    let eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
    return $ any eventIsQPress events
  
offColor = V4 0 0 0 0
onColor = V4 255 255 255 0

chip8DisplayWidth = 64 :: Int
chip8DisplayHeight = 32 :: Int
chip8TotalPixels = chip8DisplayWidth * chip8DisplayHeight
chip8DisplaySize = V2 (fromIntegral chip8DisplayWidth) (fromIntegral chip8DisplayHeight)

dummyTexture :: Renderer -> IO Texture
dummyTexture renderer = do
    texture <- createTexture renderer RGBA8888 TextureAccessStreaming chip8DisplaySize
    updateTexture texture Nothing dummyDisplay (fromIntegral (chip8DisplayWidth * 4))

dummyDisplay :: B.ByteString
dummyDisplay = B.pack $ replicate (chip8TotalPixels `div` 2 * 4) 255