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

fpsLoop:: Word32 -> (Event -> Bool) -> StateT a IO Bool 
           -> State a Bool -> (Float -> a -> IO Bool) -> a -> IO ()
fpsLoop systemFPSTime eventFunc moveIOFunc moveFunc renderFunc funcArg = do
  time <- SDL.getTicks
  runStateT (fpsLoop' time) funcArg
  return ()
    where
--      fpsLoop':: Word32 -> StateT a IO()  
      fpsLoop' prevTime = do
        curTime <- liftIO SDL.getTicks
        let loopCnt = curTime - prevTime
        prevTime' <- renderLoop loopCnt prevTime
        liftIO $ SDL.delay 1
        event <- liftIO SDL.pollEvent
        when (eventFunc event) $ fpsLoop' prevTime'

--      moveLoop:: Word32 -> Word32 -> StateT a IO Word32
      moveLoop loopCnt prevTime
        | loopCnt >= systemFPSTime = do
          moveIOFunc
          s <- get
          put $ execState moveFunc s
          moveLoop (loopCnt-systemFPSTime) (prevTime+systemFPSTime)
        | otherwise =
          return prevTime

--      renderLoop:: Word32 -> Word32 -> StateT a IO Word32 
      renderLoop loopCnt prevTime
        | loopCnt >= systemFPSTime = do
          prevTime' <- moveLoop loopCnt prevTime
          arg <- get
          liftIO $ renderFunc (1000.0 / fromIntegral loopCnt) arg
          return prevTime'
        | otherwise =
          return prevTime



