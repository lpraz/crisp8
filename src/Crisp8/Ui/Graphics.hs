{-# LANGUAGE OverloadedStrings #-}

module Crisp8.Ui.Graphics where

import SDL
import Linear(V4(..))
import qualified Data.ByteString as B
import qualified Data.Array.Unboxed as UA
import Data.Word (Word8)
import Data.Tuple (swap)

import qualified Crisp8.Machine as M

makeWindow :: IO Window
makeWindow = do
    initializeAll
    let windowConfig = defaultWindow 
          { windowInitialSize = chip8DisplaySize ^* 8 
          }
    createWindow "crisp8" windowConfig

updateDisplay :: Renderer -> M.Machine -> IO ()
updateDisplay renderer machine = do
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
getScreenByteString = B.pack . toRgb24 . UA.elems . transposeArray
    where toRgb24 = concatMap (\b -> if b then [255, 255, 255] else [0, 0, 0])

-- We have (x, y), but Haskell's (a, b) Ord gives pixels in column order
-- instead of row order when we use UA.elems.
-- Flip coords (transpose) to order correctly
-- TODO: something is wrong with this or how the display is updated. 
-- Making a blank 32x64 array works and doesn't crash
transposeArray
    :: UA.UArray (Word8, Word8) Bool
    -> UA.UArray (Word8, Word8) Bool
transposeArray = UA.ixmap
    ((0, 0), (M.displayHeight - 1, M.displayWidth - 1))
    swap

chip8DisplaySize = V2 (fromIntegral M.displayWidth) (fromIntegral M.displayHeight)