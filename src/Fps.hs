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

import Data.Word
import Graphics.UI.SDL as SDL

fpsLoop:: Word32 -> (Event -> Bool) -> (a -> IO a) -> (Float -> a -> IO a) -> a -> IO()
fpsLoop systemFPSTime eventFunc moveFunc renderFunc arg = do
    time <- SDL.getTicks
    fpsLoop' time arg
    where
        fpsLoop' prevTime arg = do
            curTime <- SDL.getTicks
            let loopCnt = curTime - prevTime

            (prevTime', arg') <- renderLoop loopCnt prevTime arg

            SDL.delay 1
            event <- SDL.pollEvent
            let ret = eventFunc event
            if ret==True then fpsLoop' prevTime' arg'
                         else return ()

        moveLoop loopCnt prevTime arg
            | loopCnt >= systemFPSTime = do
                arg' <- moveFunc arg
                moveLoop (loopCnt-systemFPSTime) (prevTime+systemFPSTime) arg'
            | otherwise =
                return (prevTime, arg)

        renderLoop loopCnt prevTime arg
            | loopCnt >= systemFPSTime = do
                (prevTime', arg') <- moveLoop loopCnt prevTime arg
                arg'' <- renderFunc (1000.0 / fromIntegral loopCnt) arg'
                return (prevTime', arg'')
            | otherwise =
                return (prevTime, arg)



