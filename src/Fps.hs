-----------------------------------------------------------------------------
--
-- Module      :  Fps
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Fps where

import Control.Monad
import Control.Monad.State
import Data.Word
import Graphics.UI.SDL as SDL

fpsLoop:: Word32 -> (Event -> Bool) -> (a -> IO a) -> 
            (a -> a) -> (Float -> a -> IO Bool) -> a -> IO ()
fpsLoop systemFPSTime eventFunc moveIOFunc moveFunc renderFunc funcArg = do
  time <- SDL.getTicks
  fpsLoop' time funcArg
  return ()
    where
--      fpsLoop':: Word32 -> a -> IO()  
      fpsLoop' prevTime arg = do
        curTime <- SDL.getTicks
        let loopCnt = curTime - prevTime
        (prevTime', arg') <- renderLoop loopCnt prevTime arg
        SDL.delay 1
        event <- liftIO SDL.pollEvent
        when (eventFunc event) $ fpsLoop' prevTime' arg'

--      moveLoop:: Word32 -> Word32 -> a -> IO (Word32, a)
      moveLoop loopCnt prevTime arg
        | loopCnt >= systemFPSTime = do
          arg' <- moveIOFunc arg
          let arg'' = moveFunc arg'
          moveLoop (loopCnt-systemFPSTime) (prevTime+systemFPSTime) arg''
        | otherwise =
          return (prevTime, arg)

--      renderLoop:: Word32 -> Word32 -> b -> IO (Word32, b) 
      renderLoop loopCnt prevTime arg
        | loopCnt >= systemFPSTime = do
          (prevTime', arg') <- moveLoop loopCnt prevTime arg
          renderFunc (1000.0 / fromIntegral loopCnt) arg'
          return (prevTime', arg')
        | otherwise =
          return (prevTime, arg)



