module Graphics where

import Graphics.UI.SDL as SDL
import Data.Word
import Util
    
data ImageObj = ImageObj {
      px :: Float,
      py :: Float,
      width :: Int,
      height :: Int,
      alpha :: Word8,
      colorkey :: Data.Word.Word32,
      image :: Surface
}

renderImage:: Surface -> Surface -> Int -> Int -> Int -> Int -> Word8 -> Data.Word.Word32 -> IO Bool
renderImage srcSurf dstSurf x y w h a ck = do
  SDL.setColorKey srcSurf [SrcColorKey, RLEAccel] $ SDL.Pixel ck
  SDL.setAlpha srcSurf [SrcAlpha] a
  SDL.blitSurface srcSurf Nothing dstSurf (Just (Rect x y w h))

renderImageObj:: Surface -> ImageObj -> IO Bool
renderImageObj mainSurf imageobj =
    renderImage (image imageobj) mainSurf (floor $ px imageobj) (floor $ py imageobj) (width imageobj) (height imageobj)  (alpha imageobj) (colorkey imageobj)
