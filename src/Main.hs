{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

import System.Random as Random
import Graphics.UI.SDL as SDL

import Util

fieldBlockMaxX=10
fieldBlockMaxY=10
fieldBlockSize=32

data ImageSet = ImageSet {
    blockA :: Surface,
    blockB :: Surface,
    blockC :: Surface,
    blockG :: Surface
}

loadImages = do
    blockA <- SDL.loadBMP "a.bmp"
    blockB <- SDL.loadBMP "b.bmp"
    blockC <- SDL.loadBMP "c.bmp"
    blockG <- SDL.loadBMP "g.bmp"
    return (ImageSet blockA blockB blockC blockG)

data BlockType =
    BlockNone | BlockGuard | BlockA | BlockB | BlockC
    deriving Eq

main :: IO ()
main = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode 640 480 32 []
     SDL.setCaption "Video Test!" "video test"
     imageSet <- loadImages
     fieldList <- initField
     eventLoop fieldList imageSet
     SDL.quit

eventLoop:: [[BlockType]] -> ImageSet -> IO()
eventLoop fieldList imageSet = do
    --SDL.delay 100
    event <- SDL.pollEvent
    mainSurf <- SDL.getVideoSurface
    SDL.fillRect mainSurf
        (Just (Rect 0 0
                (SDL.surfaceGetWidth mainSurf)
                (SDL.surfaceGetHeight mainSurf)))
        (SDL.Pixel 0x00000000)
    (mouseX, mouseY, mouseBtn) <- getMouseState
    fieldList2 <- rander fieldList mainSurf imageSet mouseX mouseY mouseBtn
    SDL.flip mainSurf
    ret <- checkEvent event
    if ret==True then eventLoop fieldList2 imageSet
                 else return ()

checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) =
    return False
checkEvent (MouseButtonUp x y _) = do
    return True
checkEvent _         =
    return True

initField:: IO [[BlockType]]
initField = do
    blocklist <- getBlockList
    return $ --putGuard $
        splitEvery fieldBlockMaxX $
            take (fieldBlockMaxX*fieldBlockMaxY) $ blocklist
    where
        putGuard:: [[BlockType]] -> [[BlockType]]
        putGuard  list = putGuardX $ putGuardY list

        putGuardY:: [[BlockType]] -> [[BlockType]]
        putGuardY list =
            map (\x -> (BlockGuard : x ++ [BlockGuard])) list

        putGuardX:: [[BlockType]] -> [[BlockType]]
        putGuardX list =
            let
                g = [(replicate (fieldBlockMaxX+2) BlockGuard)]
            in
                g ++ list ++ g


rander:: [[BlockType]] -> Surface -> ImageSet -> Int -> Int -> [MouseButton] -> IO[[BlockType]]
rander fieldList mainSurf imageSet mouseX mouseY mBtn = do
    randerFiled fieldList mainSurf imageSet
    let mouseLeft  = any (\x -> x==ButtonLeft) mBtn
    let mouseRight = any (\x -> x==ButtonRight) mBtn
    let (x, y) = mousePos2fieldPos (mouseX, mouseY)
    let list = if mouseLeft==True then eraseBlock fieldList x y else fieldList
    let list2 = if mouseRight==True then dropBlock list else list
    appendBlock list2


randerFiled:: [[BlockType]] -> Surface -> ImageSet -> IO()
randerFiled fieldList mainSurf imageSet =
    do
        randerListX fieldList
    where
        randerListX list =
            mapM_ (\(x, l) -> randerListY x l) $ zip [0,32..] list
        randerListY x list =
            mapM_ (\(y, b) -> (blitBlock x y b)) $ zip [0,32..] list

        blitBlock:: Int -> Int -> BlockType -> IO Bool
        blitBlock x y BlockA = do
            SDL.setAlpha (blockA imageSet) [SrcAlpha] 128
            SDL.blitSurface (blockA imageSet) Nothing mainSurf (Just (Rect x y 32 32))
        blitBlock x y BlockB =
            SDL.blitSurface (blockB imageSet) Nothing mainSurf (Just (Rect x y 32 32))
        blitBlock x y BlockC =
            SDL.blitSurface (blockC imageSet) Nothing mainSurf (Just (Rect x y 32 32))
        blitBlock x y BlockGuard =
            SDL.blitSurface (blockG imageSet) Nothing mainSurf (Just (Rect x y 32 32))
        blitBlock x y _ =
            return True

mousePos2fieldPos:: (Int, Int) -> (Int, Int)
mousePos2fieldPos (x, y) =
    ((devint x 32), (devint y 32))
    where
        devint a b =
            floor $ (fromIntegral (a :: Int)) / (fromIntegral (b :: Int))


dropBlock:: [[BlockType]] -> [[BlockType]]
dropBlock = map (filter (/= BlockNone))

getBlockList:: IO [BlockType]
getBlockList = do
    newStdGen
    rand <- getStdGen
    return $ map putBlock $ randomRs (0, 2) rand
    where
        putBlock:: Int -> BlockType
        putBlock 0 =  BlockA
        putBlock 1 =  BlockB
        putBlock 2 =  BlockC

appendBlock:: [[BlockType]] -> IO [[BlockType]]
appendBlock fieldList =
    mapM putBlock fieldList
    where
        putBlock list = do
            blocklist <- getBlockList
            return $ list ++ (take (fieldBlockMaxY - length list) $ blocklist)

eraseBlock:: [[BlockType]] -> Int -> Int -> [[BlockType]]
eraseBlock fieldList x y =
    foldl (\fl pos -> baseblock fl pos) fieldList
        [(x,y) | x<-[1..fieldBlockMaxX], y<-[1..fieldBlockMaxY]]
--    setBlock fieldList x y BlockNone
    where
        baseblock list pos =
            foldl (\fl dir -> eraseline fl pos dir) list [(1,0), (0,1)]

        eraseline list (x, y) (dx, dy) =
            let
                len = sameBlockNum list x y dx dy
            in
                if len >= 4 then
                    foldl (\f n -> (setBlock f (x+n*dx) (y+n*dy) BlockNone))
                          list [0..len-1]
                else
                    list

sameBlockNum::  [[BlockType]] -> Int -> Int -> Int -> Int -> Int
sameBlockNum fieldList x y dx dy =
     sameBlockNum' (x+dx) (y+dy) (getBlock fieldList x y)
    where
        sameBlockNum' x' y' b
            | 0<=x && x<fieldBlockMaxX &&
              0<=y && y<fieldBlockMaxY &&
              b == (getBlock fieldList x' y') =
                sameBlockNum' (x'+dx) (y'+dy) b
            | otherwise =
                    (abs (x'-x)) + (abs (y'-y))

        getBlock:: [[BlockType]] -> Int -> Int -> BlockType
        getBlock fieldList x y
            | 0<=x && x<fieldBlockMaxX &&
              0<=y && y<fieldBlockMaxY
                = fieldList !! x !! y
            | otherwise
                = BlockGuard

setBlock:: [[BlockType]] -> Int -> Int -> BlockType -> [[BlockType]]
setBlock fieldList x y block =
    replaceItem x fieldList (replaceItem y (fieldList!!x) block)
















