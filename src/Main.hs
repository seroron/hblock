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
import Control.Monad

import Util
import Fps

fieldLeft = 25
fieldTop  = 30

fieldBlockMaxX =10
fieldBlockMaxY =10
fieldBlockSize =38
systemFPSTime  = 1000/30

initLifeTime = 300

data ImageSet = ImageSet {
    blockA :: Surface,
    blockB :: Surface,
    blockC :: Surface,
    blockG :: Surface,
    frame  :: Surface,
    numbers :: [Surface],
    backGround :: Surface,
    timeArrow :: Surface
}

data ImageObj = ImageObj {
  px :: Float,
  py :: Float,
  width :: Int,
  height :: Int,
  alpha :: Word8,
  image :: Surface
}

data BlockType =
  BlockNone | BlockGuard | BlockA | BlockB | BlockC
  deriving (Eq, Show)

data BlockState =
  BS_Stay | BS_Dropping | BS_Removing | BS_Removed | BS_RotateRight | BS_RotateLeft
  deriving (Eq, Show)

data BlockObj = BlockObj {
  state :: BlockState,
  blocktype :: BlockType,
  imageObj :: ImageObj
}

data GameInfo = GameInfo {
      hiscore :: Integer,
      score    :: Integer,
      chain    :: Integer,
      lifetime :: Integer
}

data GameState =
    GS_Stay | GS_Dropping | GS_Removing | GS_Rotating | GS_GameOver

--nullBlockObj =
--    BlockObj (-1) (-1) BS_Stay BlockNone Nothing

main :: IO ()
main = do
  stdgen <- getStdGen
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Video Test!" "video test"
  imageSet <- loadImages
  let (fieldList, newgen) = initField imageSet stdgen
  let gameInfo = GameInfo 0 0 0 initLifeTime
  fpsLoop 33 checkEvent nextFrame renderFrame
    (GS_Removing, gameInfo, fieldList, imageSet, newgen)
  SDL.quit

loadImages:: IO ImageSet
loadImages = do
    blockA <- SDL.loadBMP "img/blue.bmp"
    blockB <- SDL.loadBMP "img/yellow.bmp"
    blockC <- SDL.loadBMP "img/purple.bmp"
    blockG <- SDL.loadBMP "img/green.bmp"
    frame  <- SDL.loadBMP "img/rotate_frame.bmp"
    numbers <- mapM SDL.loadBMP ["img/n" ++ (show a) ++ ".bmp" | a <- [0..9]]
    backGround <- SDL.loadBMP "img/background.bmp"
    timeArrow <- SDL.loadBMP "img/timearrow.bmp"
    return (ImageSet blockA blockB blockC blockG frame numbers backGround timeArrow)

checkEvent:: Event -> Bool
checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) = False
checkEvent Quit                             = False
checkEvent _                                = True


nextFrame:: (GameState, GameInfo, [[BlockObj]], ImageSet, StdGen)
             -> IO(GameState, GameInfo, [[BlockObj]], ImageSet, StdGen)
nextFrame (GS_Stay, gameInfo, fieldList, imageSet, stdgen) = do
    (mouseX, mouseY, mouseBtn) <- getMouseState
    let mouseLeft = any (== ButtonLeft) mouseBtn
    return $ nextFrame' mouseLeft mouseX mouseY
    where
        nextFrame' True mouseX mouseY = 
            let 
                (bx, by) = mousePos2fieldPos (mouseX, mouseY)
            in
              (GS_Rotating, 
               decLifeTime gameInfo 10, 
               setRotateState fieldList bx by, 
               imageSet, 
               stdgen)
        nextFrame' False _ _ =
            (GS_Stay, decLifeTime gameInfo 1, fieldList, imageSet, stdgen)

nextFrame (GS_Removing, gameInfo, fieldList, imageSet, stdgen) = do
    ret <- removeBlock fieldList
    nextFrame' ret
    where
        nextFrame' (True, list) = do
            let (newgen, list') = appendBlock imageSet stdgen list
            return (GS_Dropping, gameInfo, setDropState list', imageSet, newgen)
        nextFrame' (False, list) =
            return (GS_Removing, gameInfo, list, imageSet, stdgen)

nextFrame (GS_Dropping, gameInfo, fieldList, imageSet, stdgen) = do
    ret <- dropBlock fieldList
    nextFrame' ret
    where
        nextFrame' (True, list) = do
            let (eraseNum, list') = setRemoveState list
            if eraseNum>0 then return (GS_Removing,
                                       addEraseBounus gameInfo eraseNum,
                                       list', imageSet, stdgen)
                          else return (GS_Stay, gameInfo, list', imageSet, stdgen)
        nextFrame' (False, list) =
            return (GS_Dropping, gameInfo, list, imageSet, stdgen)

nextFrame (GS_Rotating, gameInfo, fieldList, imageSet, stdgen) = do
  ret <- rotateBlock fieldList
  return $ nextFrame' ret
    where
      nextFrame' (True, list) =
        let 
            (eraseNum, list') = setRemoveState list
        in
          (GS_Removing, addEraseBounus gameInfo eraseNum, list', imageSet, stdgen)
      nextFrame' (False, list) =
          let
              gameInfo' = decLifeTime gameInfo 1
          in
            if lifetime gameInfo' > 0 then
                (GS_Rotating, gameInfo', list, imageSet, stdgen)
            else
                (GS_GameOver, gameInfo', list, imageSet, stdgen)

nextFrame (GS_GameOver, gameInfo, fieldList, imageSet, stdgen) = do
  return (GS_GameOver, gameInfo, fieldList, imageSet, stdgen)

addEraseBounus:: GameInfo -> Int -> GameInfo
addEraseBounus gameInfo 0 =
    gameInfo{chain = 0}
addEraseBounus gameInfo eraseNum =
    gameInfo{chain = (chain gameInfo) + 1,
             score = (score gameInfo) + (toInteger eraseNum)*10*((chain gameInfo)+1),
             lifetime = (lifetime gameInfo) + (toInteger eraseNum)*((chain gameInfo)+1)}

renderFrame:: Float -> (GameState, GameInfo, [[BlockObj]], ImageSet, StdGen)
               -> IO (GameState, GameInfo, [[BlockObj]], ImageSet, StdGen)
renderFrame fps (gameState, gameInfo, fieldList, imageSet, stdgen) = do
  mainSurf <- SDL.getVideoSurface
  SDL.blitSurface (backGround imageSet) Nothing mainSurf (Just (Rect 0 0 640 320))
  renderFiled fieldList mainSurf

  renderCursor mainSurf imageSet

  renderNumer mainSurf imageSet 635  70 (hiscore gameInfo)  
  renderNumer mainSurf imageSet 635 170 (score gameInfo)
  renderNumer mainSurf imageSet 635 270 (chain gameInfo)
  renderNumer mainSurf imageSet 635 370 (lifetime gameInfo)

  -- renderNumer mainSurf imageSet 620 430 $ floor fps

  renderTimeArrow mainSurf imageSet gameInfo

  SDL.flip mainSurf
  return (gameState, gameInfo, fieldList, imageSet, stdgen)

renderCursor:: Surface -> ImageSet -> IO Bool
renderCursor mainSurf imageSet = do
  (mouseX, mouseY, mouseBtn) <- getMouseState
  let (px, py) = mousePos2fieldPos (mouseX, mouseY)
  SDL.setColorKey (frame imageSet) [SrcColorKey, RLEAccel] (SDL.Pixel 0x00000000)
  SDL.blitSurface (frame imageSet) Nothing mainSurf
        (Just (Rect (defaultBlockImgPosX px) (defaultBlockImgPosY py)
                    (fieldBlockSize*2) fieldBlockSize))

renderNumer:: Surface -> ImageSet -> Int -> Int -> Integer -> IO()
renderNumer mainSurf imageSet x y num 
    | num<=0 
        = renderNum x 0 >> return()
    | otherwise 
        = do renderNum x $ fromInteger $ num `mod` 10
             when (num>=10) $ renderNumer mainSurf imageSet (x-28) y (num `div` 10)
    where
      renderNum x' n = do
        SDL.setColorKey ((numbers imageSet)!!n) [SrcColorKey, RLEAccel] (SDL.Pixel 0x00000000)
        SDL.blitSurface ((numbers imageSet)!!n)
               Nothing mainSurf (Just (Rect (x'-28) y 28 50))
  
timeArrowTop = 436
timeArrowLeft = 110
timeArrowSize = 36
timeArrowUnitLife = 90

renderTimeArrow:: Surface -> ImageSet -> GameInfo -> IO Bool 
renderTimeArrow mainSurf imageSet gameInfo 
    | (lifetime gameInfo) <=0 
        = return True
    | otherwise = do
  SDL.setColorKey (timeArrow imageSet) [SrcColorKey, RLEAccel] (SDL.Pixel 0x00000000)
  let arrowNum = (lifetime gameInfo) `divint` timeArrowUnitLife
  mapM_ (render 255) [0..(arrowNum - 1)]
  render ((255*((lifetime gameInfo) `mod` timeArrowUnitLife)) `divint` timeArrowUnitLife)
         arrowNum
  where
    render a nx = do
       SDL.setAlpha (timeArrow imageSet) [SrcAlpha] a
       SDL.blitSurface (timeArrow imageSet) Nothing mainSurf
               (Just (Rect (timeArrowLeft+nx*timeArrowSize) timeArrowTop 
                            timeArrowSize timeArrowSize))
    
createRandomBlockObj:: Int -> Int -> StdGen -> ImageSet -> (BlockObj, StdGen)
createRandomBlockObj x y stdgen imageset =
  let
    (bt, newgen) = nextBlockType stdgen
  in
   (createBlockObj x y bt imageset, newgen)
    
createBlockObj:: Int -> Int -> BlockType -> ImageSet -> BlockObj
createBlockObj x y blocktype imageset =
  let
    image
      | blocktype == BlockA = blockA imageset
      | blocktype == BlockB = blockB imageset
      | blocktype == BlockC = blockC imageset

    imageobj = ImageObj (fromIntegral $ defaultBlockImgPosX x)
                        (fromIntegral $ defaultBlockImgPosY y)
                        fieldBlockSize fieldBlockSize 255 image
  in
    BlockObj BS_Stay blocktype imageobj

initField:: ImageSet -> StdGen -> ([[BlockObj]], StdGen)
initField imageset stdgen = 
  let
    (btl, newgen) = createBlockTypeList stdgen (fieldBlockMaxX*fieldBlockMaxY)
  in
    (splitEvery fieldBlockMaxX $
          zipWith (\(x, y) blocktype -> createBlockObj x y blocktype imageset)
                [(x, y) | x<-[0..(fieldBlockMaxX-1)], y<-[0..(fieldBlockMaxY-1)]]
                btl,
     newgen)
  

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
  mapM_ (\b -> render (imageObj b)) $ join fieldList
  where
    render:: ImageObj -> IO Bool
    render imageobj = do
      SDL.setColorKey (image imageobj) [SrcColorKey, RLEAccel] (SDL.Pixel 0x00000000)
      SDL.setAlpha (image imageobj) [SrcAlpha] (alpha imageobj)
      SDL.blitSurface (image imageobj) Nothing mainSurf
        (Just (Rect (floor $ px imageobj) (floor $ py imageobj)
                    (width imageobj) (height imageobj)))

mousePos2fieldPos:: (Int, Int) -> (Int, Int)
mousePos2fieldPos (x, y) =
    let
        bx = (x-fieldLeft-(fieldBlockSize `divint` 2)) `divint` fieldBlockSize
        by = fieldBlockMaxY - ((y-fieldTop) `divint` fieldBlockSize) - 1
    in
      (minmax 0 bx (fieldBlockMaxX-2),
       minmax 0 by (fieldBlockMaxY-1)) 
       
createBlockTypeList:: StdGen -> Int -> ([BlockType], StdGen)
createBlockTypeList stdgen num = 
  foldl (\x _ -> createBlockTypeList' x) ([], stdgen) [1..num]
    where
      createBlockTypeList' (list, gen) =
        let    
          (bt, ngen) = nextBlockType gen
        in
          (bt:list, ngen)
      
                      
nextBlockType:: StdGen -> (BlockType, StdGen)        
nextBlockType stdgen =
  let 
    (num, newgen) =  randomR (0, 2) stdgen
  in
   (putBlock num, newgen)
  where
        putBlock:: Int -> BlockType
        putBlock 0 =  BlockA
        putBlock 1 =  BlockB
        putBlock 2 =  BlockC
  

appendBlock:: ImageSet -> StdGen -> [[BlockObj]] -> (StdGen, [[BlockObj]])
appendBlock imageSet stdgen fieldList =
  let
    ((_, g), fieldList') = mapAccumL putBlock (0, stdgen) fieldList
  in
   (g, fieldList')
  where
    putBlock (x, gen) list = 
      let 
        len = length list
        newblocknum = fieldBlockMaxY - len
        (btl, newgen) = createBlockTypeList gen newblocknum
      in
       ((x+1, newgen),
        list ++ zipWith (\y b -> createBlockObj x (newblocknum + y) b imageSet) [len..] btl)
  
setDropState:: [[BlockObj]] -> [[BlockObj]]
setDropState =
    mapFieldObj setDropState'
    where
        setDropState' x y obj
            | (state obj) == BS_Stay
                && (floor $ py $ imageObj obj) < defaultBlockImgPosY y =
                obj {state = BS_Dropping}
            | otherwise =
                obj


dropBlock:: [[BlockObj]] -> IO (Bool, [[BlockObj]])
dropBlock fieldList = do
    let fieldList' = mapFieldObj dropBlock' fieldList
    let nextState  = anyFieldObj (\obj -> (state obj) == BS_Stay) fieldList'
    return (nextState, fieldList')
    where
        dropBlock' x y obj
            | (state obj) == BS_Dropping =
                let
                    imgY = py $ imageObj obj
                    defaultY = fromIntegral $ defaultBlockImgPosY y
                in
                    if imgY+6 <= defaultY then setBlockImgPosY (imgY+6) obj
                                          else obj {state = BS_Stay,
                                                 imageObj = (imageObj obj){py = defaultY}}
            | otherwise =
                obj

removeBlock:: [[BlockObj]] -> IO (Bool, [[BlockObj]])
removeBlock fieldList = do
    let fieldList'  = mapFieldObj decAplha fieldList
    let fieldList'' = map (filter (\x -> (state x) /= BS_Removed)) fieldList'
    let nextState   = anyFieldObj (\obj -> (state obj) == BS_Stay) fieldList''
    return (nextState, fieldList'')
    where
        decAplha x y blockobj
            | (state blockobj) == BS_Removing  =
                let
                    a = alpha $ imageObj blockobj
                in
                    if a >= 10 then blockobj {imageObj = ((imageObj blockobj){alpha = (a-10)})}
                    else            blockobj {imageObj = ((imageObj blockobj){alpha = 0}), state = BS_Removed}

            | otherwise =
                blockobj

rotateBlock:: [[BlockObj]] -> IO (Bool, [[BlockObj]])
rotateBlock fieldList = do
    let fieldList' = mapFieldObj rotateBlock' fieldList
    let nextState  = anyFieldObj (\obj -> (state obj) == BS_Stay) fieldList'
    return (nextState, fieldList')
    where
        rotateBlock' x y obj
            | (state obj) == BS_RotateRight =
                let
                    imgX = px $ imageObj obj
                    defaultX = fromIntegral $ defaultBlockImgPosX x
                in
                    if imgX+6 <= defaultX then obj {imageObj = ((imageObj obj){px = (imgX+6)})}
                    else                       obj {imageObj = ((imageObj obj){px = defaultX}), state = BS_Stay}
            | (state obj) == BS_RotateLeft =
                let
                    imgX = px $ imageObj obj
                    defaultX = fromIntegral $ defaultBlockImgPosX x
                in
                    if imgX-6 >= defaultX then obj {imageObj = ((imageObj obj){px = (imgX-6)})}
                    else                       obj {imageObj = ((imageObj obj){px = defaultX}), state = BS_Stay}
            | otherwise =
                obj

setRotateState:: [[BlockObj]] -> Int -> Int -> [[BlockObj]]
setRotateState fieldList x y =
    swapBlock x y (x+1) y $
        changeBlock  x    y (\obj -> obj{state = BS_RotateRight}) $
        changeBlock (x+1) y (\obj -> obj{state = BS_RotateLeft}) fieldList
--    swapBlock (x-1) y x y $
--        swapBlock x y (x+1) y $
--        changeBlock (x-1) y (\obj -> obj{state = BS_RotateRight}) $
--        changeBlock  x    y (\obj -> obj{state = BS_RotateRight}) $
--        changeBlock (x+1) y (\obj -> obj{state = BS_RotateRight}) fieldList
--    let
--        a = getBlock fieldList bx by
--        b = getBlock fieldList (bx+1) by
--    in
--        setBlock (setBlock fieldList bx by b) (bx+1) by a

setRemoveState:: [[BlockObj]] -> (Int, [[BlockObj]])
setRemoveState fieldList =
  let
    eraselist  = getEraseList
    fieldList' = foldl (\fl (x, y) -> changeBlock x y (\f -> f {state = BS_Removing}) fl)
                       fieldList eraselist
  in
    (length eraselist, fieldList')
  where
    getEraseList:: [(Int, Int)]
    getEraseList =
      nub $ foldl baseblock []
              [(x,y) | x<-[0..fieldBlockMaxX], y<-[0..fieldBlockMaxY]]

    baseblock:: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    baseblock eraselist pos =
      foldl (eraseline pos) eraselist [(1,0), (0,1)]

    eraseline:: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    eraseline (x, y) eraselist (dx, dy) =
      let
        len = sameBlockNum fieldList x y dx dy
      in
       if len >= 4 then
         foldl (\el n -> (x+n*dx,y+n*dy):el) eraselist [0..len-1]
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

changeBlock:: Int -> Int -> (BlockObj -> BlockObj) -> [[BlockObj]] -> [[BlockObj]]
changeBlock x y func fieldList =
    let
        (left, (item:right)) = splitAt y (fieldList!!x)
    in
        replaceItem x fieldList (left ++ [(func item)] ++ right)

--renumberBlock:: [[BlockObj]]  -> [[BlockObj]]
--renumberBlock =
--    mapFieldObj (\x y obj -> obj {posX = x, posY = y})

getBlock:: Int -> Int -> [[BlockObj]] -> BlockObj
getBlock x y fieldList =
     fieldList!!x!!y

setBlock:: Int -> Int -> BlockObj -> [[BlockObj]] -> [[BlockObj]]
setBlock x y obj fieldList =
    changeBlock x y (\_ -> obj) fieldList

swapBlock:: Int -> Int -> Int -> Int -> [[BlockObj]] -> [[BlockObj]]
swapBlock ax ay bx by fieldList =
    let
        a = getBlock ax ay fieldList
        b = getBlock bx by fieldList
    in
        setBlock bx by a $ setBlock ax ay b fieldList

unsetBlock:: Int -> Int -> [[BlockObj]] -> [[BlockObj]]
unsetBlock x y fieldList =
    replaceItem x fieldList (removeItem y (fieldList!!x))

decLifeTime:: GameInfo -> Integer -> GameInfo
decLifeTime gameInfo n =
    gameInfo{lifetime = (lifetime gameInfo) - n}

putFieldStr:: [[BlockObj]] -> IO()
putFieldStr =
    mapM_ putY
    where
        putY list = do
            mapM_ (\x -> putStr $ show (state x) ++ " ") list
            putStrLn ""

anyFieldObj:: (BlockObj -> Bool) -> [[BlockObj]] -> Bool
anyFieldObj func fieldList =
    foldlFieldObj (\a _ _ obj -> a && func obj) True fieldList

mapFieldObj:: (Int -> Int -> BlockObj -> BlockObj) -> [[BlockObj]] -> [[BlockObj]]
mapFieldObj func fieldList =
    zipWith mapY [0..] fieldList
    where
        mapY x list =
            zipWith (\y obj -> func x y obj) [0..] list

foldlFieldObj:: (acc -> Int -> Int -> BlockObj -> acc) -> acc -> [[BlockObj]] -> acc
foldlFieldObj func acc fieldList =
    foldl foldlY acc (zip [0..] fieldList)
    where
        foldlY accy (x, list) =
            foldl (\a (y, obj) -> func a x y obj) accy (zip [0..] list)

mapAccumLFieldObj:: (acc -> Int -> Int -> BlockObj -> (acc, BlockObj)) -> acc -> [[BlockObj]] -> (acc, [[BlockObj]])
mapAccumLFieldObj func acc fieldList =
    mapAccumL mapAccumLY acc (zip [0..] fieldList)
    where
        mapAccumLY accy (x, list) =
            mapAccumL (\a (y, obj) -> func a x y obj) accy (zip [0..] list)

setBlockImgPosY:: Float -> BlockObj -> BlockObj
setBlockImgPosY imgy obj =
    obj {imageObj = (imageObj obj) {py = imgy}}

defaultBlockImgPosX:: Int -> Int
defaultBlockImgPosX x =
    x*fieldBlockSize + fieldLeft

defaultBlockImgPosY:: Int -> Int
defaultBlockImgPosY y =
    (fieldBlockMaxY-y-1)*fieldBlockSize + fieldTop

--setBlockImgPos:: BlockObj -> BlockObj
--setBlockImgPos obj =
--    let
--        imgX = 50 + (posX obj)*32
--        imgY = 50 + (fieldBlockMaxY-(posY obj))*32
--    in
--        obj {imageObj = ((imageObj obj) {x = imgX, y = imgY})}










