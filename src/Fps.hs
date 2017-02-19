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
            (a -> Maybe a) -> (Float -> a -> IO Bool) -> a -> IO ()
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
      moveLoop loopCnt prevTime (Just arg) --TODO
        | loopCnt >= systemFPSTime = do
          arg'  <- moveIOFunc arg
          let arg'' =  moveFunc arg'
          moveLoop (loopCnt-systemFPSTime) (prevTime+systemFPSTime) arg''
        | otherwise =
          return (prevTime, arg)
                 
--      renderLoop:: Word32 -> Word32 -> b -> IO (Word32, b) 
      renderLoop loopCnt prevTime arg
        | loopCnt >= systemFPSTime = do
          (prevTime', arg') <- moveLoop loopCnt prevTime (Just arg)
          renderFunc (1000.0 / fromIntegral loopCnt) arg'
          return (prevTime', arg')
        | otherwise =
          return (prevTime, arg)


-- fpsLoop:: Word32 ->
--           (Event -> Bool) ->
--           (StateT a IO Bool) ->
--           (Float -> StateT a IO Bool) -> StateT a IO ()
-- fpsLoop systemFPSTime eventFunc moveFunc renderFunc = do
--   time <- lift SDL.getTicks
--   fpsLoop' time
--   return ()
--     where
--       fpsLoop':: Word32 -> StateT a IO ()  
--       fpsLoop' prevTime = do
--         curTime <- lift SDL.getTicks
--         let loopCnt = curTime - prevTime
--         prevTime' <- renderLoop loopCnt prevTime
--         lift $ SDL.delay 1
--         event <- liftIO SDL.pollEvent
--         when (eventFunc event) $ fpsLoop' prevTime'
           
--       moveLoop:: Word32 -> Word32 -> StateT a IO Word32
--       moveLoop loopCnt prevTime
--         | loopCnt >= systemFPSTime = do
--           moveFunc
--           moveLoop (loopCnt-systemFPSTime) (prevTime+systemFPSTime)
--         | otherwise =
--           return prevTime
                 
--       renderLoop:: Word32 -> Word32 -> StateT a IO Word32
--       renderLoop loopCnt prevTime
--         | loopCnt >= systemFPSTime = do
--           prevTime' <- moveLoop loopCnt prevTime
--           renderFunc (1000.0 / fromIntegral loopCnt)
--           return prevTime'
--         | otherwise =
--           return prevTime



