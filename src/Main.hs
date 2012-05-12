{-# LANGUAGE CPP, TemplateHaskell, StandaloneDeriving #-}
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
import Data.List
import Data.Word

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
  x :: Float,
  y :: Float,
  width :: Int,
  height :: Int,
  alpha :: Word8,
  image :: Surface
}

data BlockType =
  BlockNone | BlockGuard | BlockA | BlockB | BlockC
  deriving (Eq, Show)

data BlockState =
  BS_Stay | BS_Dropping | BS_Removing | BS_Removed
  deriving Eq

data BlockObj = BlockObj {
  posX :: Int,
  posY :: Int,
  state :: BlockState,
  blocktype :: BlockType,
  imageObj :: ImageObj
}

--nullBlockObj =
--    BlockObj (-1) (-1) BS_Stay BlockNone Nothing

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 640 480 32 []
    SDL.setCaption "Video Test!" "video test"
    imageSet <- loadImages
    fieldList <- initField imageSet
    mainLoop fieldList imageSet
    SDL.quit

loadImages = do
    blockA <- SDL.loadBMP "img/a.bmp"
    blockB <- SDL.loadBMP "img/b.bmp"
    blockC <- SDL.loadBMP "img/c.bmp"
    blockG <- SDL.loadBMP "img/g.bmp"
    return (ImageSet blockA blockB blockC blockG)

mainLoop:: [[BlockObj]] -> ImageSet -> IO()
mainLoop fieldList imageSet = do
    SDL.delay 20
    fieldList' <- nextFrame fieldList imageSet
    renderFrame fieldList'

    event <- SDL.pollEvent
    ret <- checkEvent event
    if ret==True then mainLoop fieldList' imageSet
                 else return ()

nextFrame:: [[BlockObj]] -> ImageSet -> IO[[BlockObj]]
nextFrame fieldList imageSet = do
  (mouseX, mouseY, mouseBtn) <- getMouseState
  let mouseLeft  = any (\x -> x==ButtonLeft) mouseBtn
  let mouseRight = any (\x -> x==ButtonRight) mouseBtn
  let (x, y) = mousePos2fieldPos (mouseX, mouseY)
  r <- mouse mouseLeft
  appendBlock imageSet $ setDropState $ removeBlock $
            dropBlock $ removeBlockAnim $ renumberBlock r

    where
        mouse True = do
--            putStrLn ""
--            putFieldStr fieldList
            let ret = setRemoveState fieldList
--            putStrLn ""
--            putFieldStr fieldList
            return ret

        mouse False =
            return fieldList


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

createBlockObj:: Int -> Int -> BlockType -> ImageSet -> BlockObj
createBlockObj x y blocktype imageset =
  let
    image
      | blocktype == BlockA = blockA imageset
      | blocktype == BlockB = blockB imageset
      | blocktype == BlockC = blockC imageset

    imageobj = ImageObj (fromIntegral (x*32)) (fromIntegral ((fieldBlockMaxY-y-1)*32)) 32 32 255 image
  in
    BlockObj x y BS_Stay blocktype imageobj

initField:: ImageSet -> IO [[BlockObj]]
initField imageset = do
    blocktypelist <- getBlockTypeList
    return $ splitEvery fieldBlockMaxX $
          map (\((x, y), blocktype) -> createBlockObj x y blocktype imageset)
                (zip [(x, y) | x<-[0..(fieldBlockMaxX-1)], y<-[0..(fieldBlockMaxY-1)]]
                  (take (fieldBlockMaxX*fieldBlockMaxY) $ blocktypelist))

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
  mapM_ (\b -> render (imageObj b)) $ foldl (++) [] fieldList
  where
    render:: ImageObj -> IO Bool
    render imageobj = do
      SDL.setAlpha (image imageobj) [SrcAlpha] (alpha imageobj)
      SDL.blitSurface (image imageobj) Nothing mainSurf
        (Just (Rect (floor $ x imageobj) (floor $ y imageobj)
                    (width imageobj) (height imageobj)))

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

appendBlock:: ImageSet -> [[BlockObj]] ->  IO [[BlockObj]]
appendBlock imageSet fieldList =
    mapM (\(x, list) -> putBlock x list) $ zip [0..] fieldList
    where
      putBlock x list = do
        blocktypelist <- getBlockTypeList
        let len = length list
        return $
            list ++ (
            map (\(y, b) -> createBlockObj x (fieldBlockMaxY + y-len) b imageSet) $
            zip [len..] (take (fieldBlockMaxY - len) $ blocktypelist))

setDropState:: [[BlockObj]] -> [[BlockObj]]
setDropState =
    mapFieldObj setDropState'
    where
        setDropState' px py obj
            | (state obj) == BS_Stay
                && (floor $ y $ imageObj obj) < defaultBlockImgPosY obj =
                obj {posY = py, state = BS_Dropping}
            | otherwise =
                obj


dropBlock:: [[BlockObj]] -> [[BlockObj]]
dropBlock =
    mapFieldObj dropBlock'
    where
        dropBlock' _ _ obj
            | (state obj) == BS_Dropping =
                let
                    imgY = y $ imageObj obj
                    defaultY = fromIntegral $ defaultBlockImgPosY obj
                in
                    if imgY < defaultY then setBlockImgPosY (imgY+2) obj
                    else obj {state = BS_Stay, imageObj = (imageObj obj){y = defaultY}}
            | otherwise =
                obj

removeBlock:: [[BlockObj]] -> [[BlockObj]]
removeBlock =
    map (filter (\x -> (state x) /= BS_Removed))

removeBlockAnim:: [[BlockObj]] -> [[BlockObj]]
removeBlockAnim fieldList =
    mapFieldObj removeBlockAnim' fieldList
    where
        removeBlockAnim' x y blockobj
            | (state blockobj) == BS_Removing  =
                let
                    a = alpha $ imageObj blockobj
                in
                    if a > 10 then blockobj {imageObj = ((imageObj blockobj){alpha = (a-3)})}
                    else           blockobj {imageObj = ((imageObj blockobj){alpha = 0}), state = BS_Removed}

            | otherwise =
                blockobj


setRemoveState:: [[BlockObj]] -> [[BlockObj]]
setRemoveState fieldList =
  foldl (\fl (x, y) -> changeBlock fl x y (\f -> f {state = BS_Removing})) fieldList getEraseList
  where
    getEraseList:: [(Int, Int)]
    getEraseList =
      nub $ foldl (\el pos -> baseblock pos el) []
          　　　[(x,y) | x<-[0..fieldBlockMaxX], y<-[0..fieldBlockMaxY]]

    baseblock:: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    baseblock pos eraselist =
      foldl (\el dir -> eraseline pos dir el) eraselist [(1,0), (0,1)]

    eraseline:: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    eraseline (x, y) (dx, dy) eraselist =
      let
        len = sameBlockNum fieldList x y dx dy
      in
       if len >= 4 then
         foldl (\el n -> ((x+n*dx),(y+n*dy)):el) eraselist [0..len-1]
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

changeBlock:: [[BlockObj]] -> Int -> Int -> (BlockObj -> BlockObj) -> [[BlockObj]]
changeBlock fieldList x y func =
    let
        (left, (item:right)) = splitAt y (fieldList!!x)
    in
        replaceItem x fieldList (left ++ [(func item)] ++ right)

renumberBlock:: [[BlockObj]]  -> [[BlockObj]]
renumberBlock =
    mapFieldObj (\x y obj -> obj {posX = x, posY = y})


unsetBlock:: [[BlockObj]] -> Int -> Int -> [[BlockObj]]
unsetBlock fieldList x y =
  replaceItem x fieldList (removeItem y (fieldList!!x))

putFieldStr:: [[BlockObj]] -> IO()
putFieldStr =
    mapM_ putY
    where
        putY list = do
            mapM_ (\x -> putStr $ show (alpha $ imageObj x) ++ " ") list
            putStrLn ""

mapFieldObj:: (Int -> Int -> BlockObj -> BlockObj) -> [[BlockObj]] -> [[BlockObj]]
mapFieldObj func fieldList =
    map mapY (zip [0..] fieldList)
    where
        mapY (x, list) =
            map (\(y, obj) -> func x y obj) (zip [0..] list)


setBlockImgPosY:: Float -> BlockObj -> BlockObj
setBlockImgPosY imgy obj =
    obj {imageObj = (imageObj obj) {y = imgy}}

defaultBlockImgPosY:: BlockObj -> Int
defaultBlockImgPosY obj =
    ((fieldBlockMaxY-(posY obj)-1)*32)

--setBlockImgPos:: BlockObj -> BlockObj
--setBlockImgPos obj =
--    let
--        imgX = 50 + (posX obj)*32
--        imgY = 50 + (fieldBlockMaxY-(posY obj))*32
--    in
--        obj {imageObj = ((imageObj obj) {x = imgX, y = imgY})}










