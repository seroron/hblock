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

import Data.Map (Map)
import qualified Data.Map as Map

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

data ImageObj = ImageObj {
  x :: Int
  y :: Int
  width :: Int
  height :: Int
  alpha :: Int
  image :: Surface
}

data BlockType =
  BlockNone | BlockGuard | BlockA | BlockB | BlockC
  deriving Eq

data BlockState =
  BS_Stay | BS_Dropping | BS_Removing

data BlockObj = BlockObj {
  posX :: Int
  posY :: Int
  state :: BlockState
  blocktype :: BlockType
  imageobj :: ImageObj
}

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 640 480 32 []
    SDL.setCaption "Video Test!" "video test"
    imageSet <- loadImages
    fieldList <- initField
    mainLoop fieldList imageSet
    SDL.quit

loadImages = do
    blockA <- SDL.loadBMP "a.bmp"
    blockB <- SDL.loadBMP "b.bmp"
    blockC <- SDL.loadBMP "c.bmp"
    blockG <- SDL.loadBMP "g.bmp"
    return (ImageSet blockA blockB blockC blockG)

mainLoop:: [[BlockObj]] -> ImageSet -> IO()
mainLoop fieldList imageSet = do
    SDL.delay 10
    nextFrame
    renderFrame 
      
    event <- SDL.pollEvent
    ret <- checkEvent event
    if ret==True then mainLoop fieldList imageSet
      else return ()

nextFrame:: [[BlockObj]] -> ImageSet -> IO[[BlockObj]]
nextFrame = do
  (mouseX, mouseY, mouseBtn) <- getMouseState
  let mouseLeft  = any (\x -> x==ButtonLeft) mBtn
  let mouseRight = any (\x -> x==ButtonRight) mBtn
  let (x, y) = mousePos2fieldPos (mouseX, mouseY)
  let list = if mouseLeft==True then eraseBlock fieldList else fieldList
--  let list2 = if mouseRight==True then dropBlock list else list
  appendBlock list2

renderFrame:: [[BlockObj]] -> IO()
renderFrame fieldList = do
  mainSurf <- SDL.getVideoSurface
  SDL.fillRect mainSurf
        (Just (Rect 0 0
                (SDL.surfaceGetWidth mainSurf)
                (SDL.surfaceGetHeight mainSurf)))
        (SDL.Pixel 0x00000000)
  renderFiled fieldList mainSurf
  SDL.flip mainSurf
  
checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) =
    return False
checkEvent (MouseButtonUp x y _) = do
    return True
checkEvent _         =
    return True

createBlockObj:: x -> y -> BlockType -> ImageSet -> BlockObj
createBlockObj x y blocktype imageset =
  let
    image 
      | blocktype == BlockA = blockA imageset
      | blocktype == BlockB = blockB imageset
      | blocktype == BlockC = blockC imageset
                              
    imageobj = ImageObj x*32 y*32 32 32 255 image 
  in
   BlockObj x y BS_Stay blocktype imageobj

initField:: ImageSet -> IO [[BlockObj]]
initField imageset = do
    blocktypelist <- getBlockTypeList
    return $ splitEvery fieldBlockMaxX $ 
          map 
            (\((x, y), blocktype) -> createBlockObj x y blocktype imageset) 
            zip [(x, y) | x<-[0..fieldBlockMaxX], y<-[0..fieldBlockMaxY]] $ 
              take (fieldBlockMaxX*fieldBlockMaxY) $ blocktypelist
        
    -- where
    --     putGuard:: [[BlockType]] -> [[BlockType]]
    --     putGuard  list = putGuardX $ putGuardY list

    --     putGuardY:: [[BlockType]] -> [[BlockType]]
    --     putGuardY list =
    --         map (\x -> (BlockGuard : x ++ [BlockGuard])) list

    --     putGuardX:: [[BlockType]] -> [[BlockType]]
    --     putGuardX list =
    --         let
    --             g = [(replicate (fieldBlockMaxX+2) BlockGuard)]
    --         in
    --             g ++ list ++ g


-- renderFiled:: [[BlockObj]] -> Surface -> ImageSet -> IO()
-- renderFiled fieldList mainSurf imageSet =
--     do
--         randerListX fieldList
--     where
--         renderListX list =
--             mapM_ (\(x, l) -> renderListY x l) $ zip [0,32..] list
--         renderListY x list =
--             mapM_ (\(y, b) -> (blitBlock x y b)) $ zip [0,32..] list

--         blitBlock:: Int -> Int -> BlockType -> IO Bool
--         blitBlock x y BlockA = do
--             SDL.setAlpha (blockA imageSet) [SrcAlpha] 128
--             SDL.blitSurface (blockA imageSet) Nothing mainSurf (Just (Rect x y 32 32))
--         blitBlock x y BlockB =
--             SDL.blitSurface (blockB imageSet) Nothing mainSurf (Just (Rect x y 32 32))
--         blitBlock x y BlockC =
--             SDL.blitSurface (blockC imageSet) Nothing mainSurf (Just (Rect x y 32 32))
--         blitBlock x y BlockGuard =
--             SDL.blitSurface (blockG imageSet) Nothing mainSurf (Just (Rect x y 32 32))
--         blitBlock x y _ =
--             return True

renderFiled:: [[BlockObj]] -> Surface -> IO()
renderFiled fieldList mainSurf =
  mapM_ (\b -> render (imageobj b)) $ foldl (++) [] fieldList
  where
    render imageobj =
      SDL.setAlpha (image imageobj) [SrcAlpha] (alpha imageobj)
      let rect = (Just (Rect (x imageobj) (y imageobj) (width imageobj) (height imageobj)))
      SDL.blitSurface (image imageobj) Nothing mainSurf rect

mousePos2fieldPos:: (Int, Int) -> (Int, Int)
mousePos2fieldPos (x, y) =
    ((devint x 32), (devint y 32))
    where
        devint a b =
            floor $ (fromIntegral (a :: Int)) / (fromIntegral (b :: Int))

getBlockTypeList:: IO [BlockType]
getBlockTypeList = do
    newStdGen
    rand <- getStdGen
    return $ map putBlock $ randomRs (0, 2) rand
    where
        putBlock:: Int -> BlockType
        putBlock 0 =  BlockA
        putBlock 1 =  BlockB
        putBlock 2 =  BlockC

appendBlock:: [[BlockObj]] -> ImageSet -> IO [[BlockObj]]
appendBlock fieldList imageSet =
    foldM putBlock 0 fieldList
    where
      putBlock x list = do
        blocktypelist <- getBlockTypeList
        createBlockObj x y blocktype imageSet 
            return $ list ++ (take (fieldBlockMaxY - length list) $ blocktypelist)

eraseBlock:: [[BlockObj]] -> [[BlockObj]]
eraseBlock fieldList x y =
  foldl (\fl (x, y) -> unsetBlock fl x y) fieldList getEraseList  
  where
    getEraseList:: [(Int, Int)]
    getEraseList = 
      sort $ uniq $ 
          foldl (\el pos -> baseblock pos el) []
          [(x,y) | x<-[1..fieldBlockMaxX], y<-[1..fieldBlockMaxY]]

    baseblock:: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    baseblock pos eraselist =
      foldl (\el dir -> eraseline pos dir el) eraselist [(1,0), (0,1)]

    eraseline:: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    eraseline (x, y) (dx, dy) eraselist =
      let
        len = sameBlockNum fieldList x y dx dy
      in
       if len >= 4 then
         eraselist ++ (foldl (\f n -> ((x+n*dx),(y+n*dy))) [0..len-1])
       else
         eraselist

sameBlockNum::  [[BlockObj]] -> Int -> Int -> Int -> Int -> Int
sameBlockNum fieldList x y dx dy =
  sameBlockNum' (x+dx) (y+dy) (getBlock fieldList x y)
    where
      sameBlockNum':: Int -> Int -> BlockType -> Int
      sameBlockNum' x' y' b
        | 0<=x && x<fieldBlockMaxX &&
          0<=y && y<fieldBlockMaxY &&
          b == (getBlock fieldList x' y') =
            sameBlockNum' (x'+dx) (y'+dy) b
        | otherwise =
            (abs (x'-x)) + (abs (y'-y))

      getBlock:: [[BlockObj]] -> Int -> Int -> BlockType
      getBlock fieldList x y
        | 0<=x && x<fieldBlockMaxX &&
          0<=y && y<fieldBlockMaxY = 
            blocktype (fieldList !! x !! y)
        | otherwise = 
            BlockGuard

setBlock:: [[BlockType]] -> Int -> Int -> BlockType -> [[BlockType]]
setBlock fieldList x y block =
  replaceItem x fieldList (replaceItem y (fieldList!!x) block)
  
unsetBlock:: [[BlockObj]] -> Int -> Int -> [[BlockObj]]
unsetBlock fieldList x y 
  replaceItem x fieldList (removeItem y (fieldList!!x))
 














