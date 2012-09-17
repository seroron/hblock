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
import Control.Monad.State

import Util
import Fps

fieldLeft = 25
fieldTop  = 30

fieldBlockMaxX =10
fieldBlockMaxY =10
fieldBlockSize =38
systemFPSTime  = 1000/30

initLifeTime = 300

maxLifeTime = toInteger 500

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
      blockstate :: BlockState,
      blocktype :: BlockType,
      imageObj :: ImageObj
}

data GameState =
    GS_Stay | GS_Dropping | GS_Removing | GS_Rotating | GS_GameOver
    deriving (Eq, Show)

type FieldBlock = [[BlockObj]]

data GameArgs = GameArgs {
      gameState :: GameState,
      hiScore :: Integer,
      score    :: Integer,
      chain    :: Integer,
      lifeTime :: Integer,
      fieldBlock :: FieldBlock, 
      imageSet :: ImageSet, 
      stdGen :: StdGen,
      mouseX :: Int, 
      mouseY :: Int, 
      mouseBtn :: [MouseButton]
}

--nullBlockObj =
--    BlockObj (-1) (-1) BS_Stay BlockNone Nothing

main :: IO ()
main = do
  stdgen <- getStdGen
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Video Test!" "video test"
  imageSet <- loadImages
  let (fieldBlock, newgen) = initField imageSet stdgen
  fpsLoop 33 checkEvent ioFrame nextFrame renderFrame $
          GameArgs GS_Stay 0 0 0 initLifeTime fieldBlock imageSet newgen 0 0 []
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

ioFrame:: GameArgs -> IO GameArgs
ioFrame gameargs = do
    (x, y, b) <- getMouseState
    return gameargs{mouseX = x, mouseY = y, mouseBtn = b}

nextFrame:: GameArgs -> Maybe GameArgs 
nextFrame gameargs 
  | (gameState gameargs) == GS_Stay = 
    case (any (== ButtonLeft) $ mouseBtn gameargs) of
      True -> 
        let 
          (bx, by) = mousePos2fieldPos (mouseX gameargs, mouseY gameargs)
          fb = setRotateState bx by $ fieldBlock gameargs
        in
         return $ 
           decLifeTime 10 $
           gameargs{fieldBlock = fb}
      False ->
        return gameargs

  | (gameState gameargs) == GS_Removing =
    case (removeBlock $ fieldBlock gameargs) of
      (True, fb) ->
        let 
          (fb', gen) = appendBlock fb (stdGen gameargs) (imageSet gameargs) 
        in
         return gameargs{gameState = GS_Dropping,
                         fieldBlock = fb',
                         stdGen = gen}
      (False, fb) ->
        return gameargs{fieldBlock = fb}

  | (gameState gameargs) == GS_Dropping = 
    case (dropBlock $ fieldBlock gameargs) of
      (True, fb) ->
        let 
            (eraseNum, fb') = setRemoveState fb
        in
          if eraseNum>0 then
            return $
              addEraseBounus eraseNum $ 
              gameargs{gameState = GS_Removing,
                       fieldBlock = fb'}
          else 
            return gameargs{gameState = GS_Stay,
                            fieldBlock = fb'}
      (False, fb) ->
        return gameargs{fieldBlock = fb}

  | (gameState gameargs) == GS_Rotating = 
    case (rotateBlock $ fieldBlock gameargs) of
      (True, fb) ->
        let 
            (eraseNum, fb') = setRemoveState fb
        in
         if eraseNum>0 then do 
           return $
             addEraseBounus eraseNum $
             gameargs{gameState = GS_Removing,
                      fieldBlock = fb'}
          else 
           return gameargs{gameState = GS_Stay,
                           fieldBlock = fb'}
      (False, _) -> do
        return $ decLifeTime 1 gameargs

  | (gameState gameargs) == GS_GameOver = 
    return gameargs

addEraseBounus:: Int -> GameArgs -> GameArgs
addEraseBounus 0 args =
  args{chain = 0}
addEraseBounus eraseNum args =
  args{chain    = (chain args) + 1,
       score    = (score args) + (toInteger eraseNum)*10*((chain args)+1),
       lifeTime = min maxLifeTime $
                      (lifeTime args) + (toInteger eraseNum)*((chain args)+1)}

renderFrame:: Float -> GameArgs -> IO Bool
renderFrame fps gameargs = do
  mainSurf <- SDL.getVideoSurface
  let imageset = imageSet gameargs
  
  renderBackGround mainSurf imageset
  renderFiledBlock mainSurf (fieldBlock gameargs)
  renderCursor mainSurf imageset

  renderNumer mainSurf imageset 635  70 (hiScore gameargs)  
  renderNumer mainSurf imageset 635 170 (score gameargs)
  renderNumer mainSurf imageset 635 270 (chain gameargs)
  renderNumer mainSurf imageset 635 370 (lifeTime gameargs)

  -- renderNumer mainSurf imageSet 620 430 $ floor fps

  renderTimeArrow mainSurf imageset (lifeTime gameargs)

  SDL.flip mainSurf
  return True

renderBackGround:: Surface -> ImageSet -> IO Bool
renderBackGround mainSurf imageset =
  SDL.blitSurface (backGround imageset) Nothing mainSurf (Just (Rect 0 0 640 320))

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

renderTimeArrow:: Surface -> ImageSet -> Integer -> IO Bool 
renderTimeArrow mainSurf imageset lifetime
    | lifetime <=0 
        = return True
    | otherwise = do
       SDL.setColorKey (timeArrow imageset) [SrcColorKey, RLEAccel] (SDL.Pixel 0x00000000)
       let arrowNum = lifetime `divint` timeArrowUnitLife
       mapM_ (render 255) [0..(arrowNum - 1)]
       render ((255*(lifetime `mod` timeArrowUnitLife)) `divint` timeArrowUnitLife)
                  arrowNum
  where
    render a nx = do
       SDL.setAlpha (timeArrow imageset) [SrcAlpha] a
       SDL.blitSurface (timeArrow imageset) Nothing mainSurf
               (Just (Rect (timeArrowLeft+nx*timeArrowSize) timeArrowTop 
                            timeArrowSize timeArrowSize))
    
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

createRandomBlockObj:: Int -> Int -> ImageSet -> State StdGen BlockObj
createRandomBlockObj x y imageset = do
  bt <- nextBlockType
  return $ createBlockObj x y bt imageset
  where
    nextBlockType:: State StdGen BlockType
    nextBlockType = do
      let (num, newgen) =  randomR (0, 2) $ get
      put newgen
      return $ putBlock num  

    putBlock:: Int -> BlockType
    putBlock 0 =  BlockA
    putBlock 1 =  BlockB
    putBlock 2 =  BlockC


initField:: ImageSet -> StdGen -> (FieldBlock, StdGen)
initField imageset stdgen =
    runState initField' stdgen
    where
      initField':: State StdGen FieldBlock
      initField' =
          return [[createRandomBlockObj x y imageset | x<-[0..(fieldBlockMaxX-1)]] | y<-[0..(fieldBlockMaxY-1)]]

  -- let
  --   (btl, newgen) = createBlockTypeList stdgen (fieldBlockMaxX*fieldBlockMaxY)
  -- in
  --   (splitEvery fieldBlockMaxX $
  --         zipWith (\(x, y) blocktype -> createBlockObj x y blocktype imageset)
  --               [(x, y) | x<-[0..(fieldBlockMaxX-1)], y<-[0..(fieldBlockMaxY-1)]]
  --               btl,
  --    newgen)
  

-- renderFiled:: FieldBlock -> Surface -> ImageSet -> IO()
-- renderFiled fieldBlock mainSurf imageSet =
--     do
--         randerListX fieldBlock
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

renderFiledBlock:: Surface -> FieldBlock -> IO Bool
renderFiledBlock mainSurf fieldBlock =
  fmap (any (==True)) $ mapM (\b -> render (imageObj b)) $ join fieldBlock
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
       
-- createBlockTypeList:: Int -> State GameArgs [BlockType]
-- createBlockTypeList num = do 
--   mapM (\_ -> nextBlockType) [1..num]      
                        
setRotateState:: Int -> Int -> FieldBlock -> FieldBlock
setRotateState x y fieldBlock = 
  swapBlock x y (x+1) y 
    $ changeBlock  x    y (\obj -> obj{blockstate = BS_RotateRight})
    $ changeBlock (x+1) y (\obj -> obj{blockstate = BS_RotateLeft}) fieldBlock

appendBlock:: FieldBlock -> StdGen -> ImageSet -> (FieldBlock, StdGen)
appendBlock fieldBlock stdgen imageset = 
  runState (appendBlock' fieldBlock) stdgen
  where
    appendBlock':: FieldBlock -> State StdGen FieldBlock
    appendBlock' fieldBlock = 
      zipWithM putBlock fieldBlock [0..]
    
    putBlock:: [BlockObj] -> Int -> State StdGen [BlockObj]
    putBlock list x = do
      al <- mapM (\y -> createRandomBlockObj x y imageset) [(length list)..fieldBlockMaxY]
      return $ list ++ al
    
setDropState:: FieldBlock -> FieldBlock
setDropState =
    mapFieldObj setDropState'
    where
        setDropState' x y obj
            | (blockstate obj) == BS_Stay
                && (floor $ py $ imageObj obj) < defaultBlockImgPosY y =
                obj {blockstate = BS_Dropping}
            | otherwise =
                obj

dropBlock:: FieldBlock -> (Bool, FieldBlock)
dropBlock fieldBlock = 
  let 
    fieldBlock' = mapFieldObj dropBlock' fieldBlock
    nextState   = anyFieldObj (\obj -> (blockstate obj) == BS_Stay) fieldBlock'
  in
   (nextState, fieldBlock')
     where
        dropBlock' x y obj
            | (blockstate obj) == BS_Dropping =
                let
                    imgY = py $ imageObj obj
                    defaultY = fromIntegral $ defaultBlockImgPosY y
                in
                    if imgY+6 <= defaultY then setBlockImgPosY (imgY+6) obj
                                          else obj {blockstate = BS_Stay,
                                                 imageObj = (imageObj obj){py = defaultY}}
            | otherwise =
                obj

removeBlock:: FieldBlock -> (Bool, FieldBlock)
removeBlock fieldBlock =
    let 
      fieldBlock' = map (filter (\x -> (blockstate x) /= BS_Removed)) 
                      $ mapFieldObj decAplha fieldBlock
      nextState   = anyFieldObj (\obj -> (blockstate obj) == BS_Stay) fieldBlock'
    in
      (nextState, fieldBlock')
    where
        decAplha x y blockobj
            | (blockstate blockobj) == BS_Removing  =
                let
                    a = alpha $ imageObj blockobj
                in
                    if a >= 10 then blockobj {imageObj = ((imageObj blockobj){alpha = (a-10)})}
                    else            blockobj {imageObj = ((imageObj blockobj){alpha = 0}), blockstate = BS_Removed}

            | otherwise =
                blockobj

rotateBlock:: FieldBlock -> (Bool, FieldBlock)
rotateBlock fieldBlock =
    let 
      fieldBlock' = mapFieldObj rotateBlock' fieldBlock
      nextState  = anyFieldObj (\obj -> (blockstate obj) == BS_Stay) fieldBlock'
    in
      (nextState, fieldBlock')
    where
        rotateBlock' x y obj
            | (blockstate obj) == BS_RotateRight =
                let
                    imgX = px $ imageObj obj
                    defaultX = fromIntegral $ defaultBlockImgPosX x
                in
                    if imgX+6 <= defaultX then obj {imageObj = ((imageObj obj){px = (imgX+6)})}
                    else                       obj {imageObj = ((imageObj obj){px = defaultX}), blockstate = BS_Stay}
            | (blockstate obj) == BS_RotateLeft =
                let
                    imgX = px $ imageObj obj
                    defaultX = fromIntegral $ defaultBlockImgPosX x
                in
                    if imgX-6 >= defaultX then obj {imageObj = ((imageObj obj){px = (imgX-6)})}
                    else                       obj {imageObj = ((imageObj obj){px = defaultX}), blockstate = BS_Stay}
            | otherwise =
                obj

setRemoveState:: FieldBlock -> (Int, FieldBlock)
setRemoveState fieldBlock =
  let
    eraselist  = getEraseList
    fieldBlock' = foldl (\fl (x, y) -> changeBlock x y (\f -> f {blockstate = BS_Removing}) fl)
                       fieldBlock eraselist
  in
    (length eraselist, fieldBlock')
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
        len = sameBlockNum fieldBlock x y dx dy
      in
       if len >= 4 then
         foldl (\el n -> (x+n*dx,y+n*dy):el) eraselist [0..len-1]
       else
         eraselist

sameBlockNum::  FieldBlock -> Int -> Int -> Int -> Int -> Int
sameBlockNum fieldBlock x y dx dy =
  sameBlockNum' (x+dx) (y+dy) (getBlock fieldBlock x y)
    where
      sameBlockNum':: Int -> Int -> BlockType -> Int
      sameBlockNum' x' y' b
        | 0<=x && x<fieldBlockMaxX &&
          0<=y && y<fieldBlockMaxY &&
          b == (getBlock fieldBlock x' y') =
            sameBlockNum' (x'+dx) (y'+dy) b
        | otherwise =
            (abs (x'-x)) + (abs (y'-y))

      getBlock:: FieldBlock -> Int -> Int -> BlockType
      getBlock fieldBlock x y
        | 0<=x && x<fieldBlockMaxX &&
          0<=y && y<fieldBlockMaxY =
            blocktype (fieldBlock !! x !! y)
        | otherwise =
            BlockGuard

changeBlock:: Int -> Int -> (BlockObj -> BlockObj) -> FieldBlock -> FieldBlock
changeBlock x y func fieldBlock =
    let
        (left, (item:right)) = splitAt y (fieldBlock!!x)
    in
        replaceItem x fieldBlock (left ++ [(func item)] ++ right)

--renumberBlock:: FieldBlock  -> FieldBlock
--renumberBlock =
--    mapFieldObj (\x y obj -> obj {posX = x, posY = y})

getBlock:: Int -> Int -> FieldBlock -> BlockObj
getBlock x y fieldBlock =
     fieldBlock!!x!!y

setBlock:: Int -> Int -> BlockObj -> FieldBlock -> FieldBlock
setBlock x y obj fieldBlock =
    changeBlock x y (\_ -> obj) fieldBlock

swapBlock:: Int -> Int -> Int -> Int -> FieldBlock -> FieldBlock
swapBlock ax ay bx by fieldBlock =
    let
        a = getBlock ax ay fieldBlock
        b = getBlock bx by fieldBlock
    in
        setBlock bx by a $ setBlock ax ay b fieldBlock

unsetBlock:: Int -> Int -> FieldBlock -> FieldBlock
unsetBlock x y fieldBlock =
    replaceItem x fieldBlock (removeItem y (fieldBlock!!x))

decLifeTime:: Integer -> GameArgs -> GameArgs
decLifeTime n gameargs =
    let 
        lt = (lifeTime gameargs) - n
    in
      if lt > 0 then gameargs{lifeTime = lt}
                else gameargs{lifeTime  = 0,
                              gameState = GS_GameOver}

putFieldStr:: FieldBlock -> IO()
putFieldStr =
    mapM_ putY
    where
        putY list = do
            mapM_ (\x -> putStr $ show (blockstate x) ++ " ") list
            putStrLn ""

anyFieldObj:: (BlockObj -> Bool) -> FieldBlock -> Bool
anyFieldObj func fieldBlock =
    foldlFieldObj (\a _ _ obj -> a && func obj) True fieldBlock

mapFieldObj:: (Int -> Int -> BlockObj -> BlockObj) -> FieldBlock -> FieldBlock
mapFieldObj func fieldBlock =
    zipWith mapY [0..] fieldBlock
    where
        mapY x list =
            zipWith (\y obj -> func x y obj) [0..] list

foldlFieldObj:: (acc -> Int -> Int -> BlockObj -> acc) -> acc -> FieldBlock -> acc
foldlFieldObj func acc fieldBlock =
    foldl foldlY acc (zip [0..] fieldBlock)
    where
        foldlY accy (x, list) =
            foldl (\a (y, obj) -> func a x y obj) accy (zip [0..] list)

mapAccumLFieldObj:: (acc -> Int -> Int -> BlockObj -> (acc, BlockObj)) -> acc -> FieldBlock -> (acc, FieldBlock)
mapAccumLFieldObj func acc fieldBlock =
    mapAccumL mapAccumLY acc (zip [0..] fieldBlock)
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










