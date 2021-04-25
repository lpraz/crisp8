module Crisp8.Ui.Graphics where

import SDL
import Linear(V4(..))
import qualified Data.ByteString as B
import qualified Data.Array.Unboxed as UA
import Data.Word (Word8)

import qualified Crisp8.Machine as M

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

offColor = V4 0 0 0 0
onColor = V4 255 255 255 0

chip8DisplaySize = V2 (fromIntegral M.displayWidth) (fromIntegral M.displayHeight)