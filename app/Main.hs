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

module Main where

import System.Random as Random
import Graphics.UI.SDL as SDL
import Data.List
import Data.Lists
import Data.Word
import Data.Ord
import Data.MultiSet (MultiSet)
import Control.Monad
import Control.Monad.State
import Debug.Trace
import Control.Lens
    
import Util
import Fps
import Graphics

fieldBlockMaxX =10
fieldBlockMaxY =10
fieldBlockSize =38

fieldLeft = 25
fieldTop  = 30
fieldRight = fieldLeft + fieldBlockMaxX*fieldBlockSize
fieldBottom = fieldTop + fieldBlockMaxY*fieldBlockSize
                 
systemFPSTime  = 1000/30

initLifeTime = 30

maxLifeTime = toInteger 500

timeArrowTop = 436
timeArrowLeft = 110
timeArrowSize = 36
timeArrowUnitLife = 90

                    
data ImageSet = ImageSet {
      blockA :: Surface,
      blockB :: Surface,
      blockC :: Surface,
      blockG :: Surface,
      frame  :: Surface,
      numbers :: [Surface],
      backGround :: Surface,
      timeArrow :: Surface,
      blackBG :: Surface,
      gameover :: Surface,
      retry :: Surface
    }


data BlockType =
    BlockNone | BlockG | BlockA | BlockB | BlockC | BlockWall
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
    GS_Stay | 
    GS_Dropping | 
    GS_Removing | 
    GS_Rotating | 
    GS_GameOver | 
    GS_Retry
    deriving (Eq, Show)

type FieldBlock = [[BlockObj]]

-- data GameObject =
--     GameOverBGObj {
--       count :: Int
--     } |
--     GameOverMsgObj {
--       retry :: Bool,
--       exit  :: Bool
--     }
--     deriving(Ord)

-- instance Ord GameObject
--     compare x y =
--         (priority x) - (priority y)
--     where
--       priority _@GameOverBGObj{}  = 10
--       priority _@GameOverMsgObj{} = 20

data GameArgs = GameArgs {
      gameState :: GameState,
      hiScore :: Integer,
      score    :: Integer,
      chain    :: Integer,
      level    :: Integer,
      eraseCnt  :: Integer,
      lifeTime :: Integer,
      fieldBlock :: FieldBlock, 
      imageSet :: ImageSet, 
      stdGen :: StdGen,
      mouseX :: Int, 
      mouseY :: Int, 
      mouseBtn :: [MouseButton],
      gameScenes :: [GameScene],
      removedSceneTypes :: [GameSceneType],
      appendedScenes :: [GameScene]
}

class GameObject gobj where
    move ::  gobj -> State GameArgs GameScene
    render :: gobj -> GameArgs -> IO Bool

data GameSceneType =
    Gst_GameMainScene |
    Gst_GameOverScene |
    Gst_RetryScene
    deriving (Show, Enum, Eq, Ord)
              
data GameScene =
    GameMainScene {
      sceneType :: GameSceneType
    } |
    GameOverScene {
      sceneType :: GameSceneType,
      gos_backPlane :: ImageObj,
      gos_gameOverImgObj :: ImageObj
    } |
    RetryScene {
      sceneType :: GameSceneType,
      gos_retryImgObj :: ImageObj
    }

--makeLenses ''GameScene

newGameMainScene :: GameScene
newGameMainScene =
    GameMainScene {sceneType          = Gst_GameMainScene}

newGameOverScene :: ImageSet -> GameScene
newGameOverScene imageSet = 
    GameOverScene {sceneType          = Gst_GameOverScene, 
                   gos_backPlane      = blackImageObj imageSet,
                   gos_gameOverImgObj = ImageObj 150 150 640 480 255 0x00000000 (gameover imageSet)}

newRetryScene :: ImageSet -> GameScene
newRetryScene imageSet = 
    RetryScene{sceneType = Gst_RetryScene,
               gos_retryImgObj    = ImageObj 185 300 640 480 255 0x00000000 (retry imageSet)}

appendScene :: GameScene -> State GameArgs ()
appendScene gs =
    modify $ \gameargs -> gameargs{appendedScenes = gs:(appendedScenes gameargs)}

removeScene :: GameSceneType -> State GameArgs ()
removeScene gst =
    modify $ \gameargs -> gameargs{removedSceneTypes = gst:(removedSceneTypes gameargs)}
    
instance GameObject GameScene where
    move gs@GameMainScene{} = do
      gameargs <- get
      case gameState gameargs of
        GS_Stay -> 
            gameStateStay
        GS_Removing ->
            gameStateRemoving
        GS_Dropping -> 
            gameStateDropping
        GS_Rotating -> 
            gameStateRotating
        GS_GameOver ->
            gameStateGameOver
        GS_Retry -> 
            return True
      return gs
               
    move gs@GameOverScene{}
         | (alpha $ gos_backPlane gs) < 155 =
             do
               let nalpha = min 155 $ (alpha $ gos_backPlane gs) + 10
               let ngs = gs {gos_backPlane = (gos_backPlane gs){alpha = nalpha}}
               gameargs <- get
               when(nalpha == 155) $ appendScene $ newRetryScene $ imageSet gameargs
               return ngs
         | otherwise = do
               return gs
        
    move gs@RetryScene{} = do
      gameargs <- get
      case any (== ButtonLeft) $ mouseBtn gameargs of
        True -> do
            removeScene Gst_GameOverScene
            removeScene Gst_RetryScene
            modify resetGameArgs
            initField
            return gs
        False ->
            return gs

    render gs@GameMainScene{} gameargs = do
      mainSurf <- SDL.getVideoSurface
      let imageset = imageSet gameargs
  
      renderBackGround mainSurf imageset
      renderFiledBlock mainSurf (fieldBlock gameargs)
      renderCursor mainSurf imageset

      renderNumer mainSurf imageset 635  70 (hiScore gameargs)  
      renderNumer mainSurf imageset 635 170 (score gameargs)
      renderNumer mainSurf imageset 635 270 (chain gameargs)
      renderNumer mainSurf imageset 635 370 (eraseCnt gameargs)

      -- renderNumer mainSurf imageset 635 420 (fromIntegral $ length $ gameScenes gameargs)
      -- renderNumer mainSurf imageSet 620 430 $ floor fps

      renderTimeArrow mainSurf imageset (lifeTime gameargs)

      return True
                                
    render gs@GameOverScene{} gameargs = do
        mainSurf <- SDL.getVideoSurface
        renderImageObj mainSurf (gos_backPlane gs)
        renderImageObj mainSurf (gos_gameOverImgObj gs) 
        return True
        
    render gs@RetryScene{} gameargs = do
      mainSurf <- SDL.getVideoSurface
      renderImageObj mainSurf (gos_retryImgObj gs) 
      return True
                    
--nullBlockObj =
--    BlockObj (-1) (-1) BS_Stay BlockNone Nothing

blackImageObj imageset =
    ImageObj 0 0 640 480 0 0x00000000 (blackBG imageset)

initGameArgs :: ImageSet -> StdGen -> GameArgs
initGameArgs imageset stdgen =
    GameArgs {
         gameState = GS_Removing,
         hiScore    = 0,
         score      = 0,
         chain      = 0,
         level      = 1,
         eraseCnt   = 0,
         lifeTime   = initLifeTime,
         fieldBlock = [[]],
         imageSet   = imageset,
         stdGen     = stdgen,
         mouseX     = 0,
         mouseY     = 0,
         mouseBtn   = [],
         gameScenes = [],
         removedSceneTypes = [],
         appendedScenes = []
       }

resetGameArgs :: GameArgs -> GameArgs
resetGameArgs gameargs =
    gameargs {
         gameState = GS_Removing,
         score      = 0,
         chain      = 0,
         level      = 1,
         eraseCnt   = 0,
         lifeTime   = initLifeTime,
         fieldBlock = [[]]
}
    
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "hblock" "hblock"
     
  imageset <- loadImages
  stdgen <- getStdGen
  let ga = execState initGame $ initGameArgs imageset stdgen
  fpsLoop 33 checkEvent ioFrame nextFrame renderFrame ga
  SDL.quit
     
loadImages:: IO ImageSet
loadImages =
    ImageSet
    <$> SDL.loadBMP "img/blue.bmp"
            <*> SDL.loadBMP "img/yellow.bmp"
            <*> SDL.loadBMP "img/purple.bmp"
            <*> SDL.loadBMP "img/guard.bmp"
            <*> SDL.loadBMP "img/rotate_frame.bmp"
            <*> mapM SDL.loadBMP ["img/n" ++ (show a) ++ ".bmp" | a <- [0..9]]
            <*> SDL.loadBMP "img/background.bmp"
            <*> SDL.loadBMP "img/timearrow.bmp"
            <*> SDL.loadBMP "img/black.bmp"
            <*> SDL.loadBMP "img/gameover.bmp"
            <*> SDL.loadBMP "img/retry.bmp"

checkEvent:: Event -> Bool
checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) = False
checkEvent Quit                             = False
checkEvent _                                = True

ioFrame:: GameArgs -> IO GameArgs
ioFrame gameargs = do
    (x, y, b) <- getMouseState
    return gameargs{mouseX = x, mouseY = y, mouseBtn = b}

mouseBtnLeft :: GameArgs -> Bool
mouseBtnLeft gameargs = 
    any (== ButtonLeft) $ mouseBtn gameargs
        
gameStateStay:: State GameArgs Bool
gameStateStay = do
  gameargs <- get
  case (mouseBtnLeft gameargs) of
    True -> do 
      modifyFieldBlock $ setBlockRotateState $ mousePos2fieldPos (mouseX gameargs, mouseY gameargs)
      decLifeTime 10
      setGameState GS_Rotating
    False ->
      decLifeTime 1
  return True

gameStateRemoving:: State GameArgs Bool
gameStateRemoving = do
  modifyFieldBlock removeBlock
  fb <- getFieldBlock
  case (anyFieldObjState BS_Stay fb) of     
    True -> do
      appendBlock
      modifyFieldBlock setDropState
      setGameState GS_Dropping
    False ->
      return ()
  return True

gameStateDropping:: State GameArgs Bool
gameStateDropping = do
  modifyFieldBlock dropBlock
  fb <- getFieldBlock
  nextState $ anyFieldObjState BS_Stay fb
            
  where
    nextState:: Bool -> State GameArgs Bool
    nextState True = do
      eraseNum <- setRemoveState
      if eraseNum>0 then
          do
            incEraseCnt
            setLevel
            addEraseBounus eraseNum
            setGameState GS_Removing
      else 
          setGameState GS_Stay
      return True
             
    nextState _ =
        return True
                
    dropBlock:: FieldBlock -> FieldBlock
    dropBlock fieldBlock = 
        mapFieldObj dropBlock' fieldBlock
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


gameStateRotating:: State GameArgs Bool
gameStateRotating = do
  modifyFieldBlock rotateBlock
  fb <- getFieldBlock
  nextState $ anyFieldObjState BS_Stay fb

  where
    nextState:: Bool -> State GameArgs Bool
    nextState True = do
      eraseNum <- setRemoveState
      if eraseNum>0 then
          do
            incEraseCnt
            setLevel
            addEraseBounus eraseNum
            setGameState GS_Removing
      else 
          setGameState GS_Stay
      return True
             
    nextState _ =
        return True


    rotateBlock:: FieldBlock -> FieldBlock
    rotateBlock fb =
        mapFieldObj rotateBlock' fb
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

gameStateGameOver:: State GameArgs Bool
gameStateGameOver = do
  return True
         
nextFrame:: GameArgs -> Maybe GameArgs
nextFrame ga =
  return $ execState moveScenes ga 

execScenes :: (GameScene -> State GameArgs GameScene) -> State GameArgs ()
execScenes f = do
  gameargs <- get
  ngs <- mapM f (gameScenes gameargs)
  ngss <- nextGameScenes ngs
  modify $ \ga -> ga{gameScenes = ngss,
                    removedSceneTypes = [],
                    appendedScenes = []}
    where
      nextGameScenes :: [GameScene] -> State GameArgs [GameScene]
      nextGameScenes ngs = do
            gameargs <- get
            return $ mergeBy (comparing sceneType) (sortBy (comparing sceneType) $ appendedScenes gameargs) $
                           remove (removedSceneTypes gameargs) ngs

      remove :: [GameSceneType] -> [GameScene] -> [GameScene]
      remove gst gs = 
          foldr (\a b -> filter (\x -> a /= (sceneType x)) b) gs gst
          
moveScenes :: State GameArgs ()
moveScenes = execScenes move

renderScenes :: GameArgs -> IO ()
renderScenes gameargs =
    mapM_  (\x -> render x gameargs) (gameScenes gameargs)                 
             
addEraseBounus:: Int -> State GameArgs ()
addEraseBounus 0 =
    modify $ \gameargs -> gameargs{chain = 0}
addEraseBounus eraseNum =
    modify
    $ \gameargs -> gameargs{chain    = (chain gameargs) + 1,
                            score    = (score gameargs) + (toInteger eraseNum)*10*((chain gameargs)+1),
                            lifeTime = min maxLifeTime $
                                       (lifeTime gameargs) + (toInteger eraseNum)*((chain gameargs)+1)}

clearChain:: State GameArgs ()
clearChain = do
  modify $ \gameargs -> gameargs{chain = 0}

incChain:: State GameArgs ()
incChain = do
  modify $ \gameargs -> gameargs{chain = (chain gameargs) + 1}
                        
renderFrame:: Float -> GameArgs -> IO Bool
renderFrame fps gameargs = do
  renderScenes gameargs
               
  mainSurf <- SDL.getVideoSurface
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

-- renderGameOver:: Surface -> ImageSet -> GameArgs -> IO Bool     
-- renderGameOver mainSurf imageset gameargs 
--     | (gameState gameargs) == GS_GameOver = 
--         render
--     | (gameState gameargs) == GS_Retry =
--         render
--     | otherwise = 
--         return True

--     where
--       render = do
--           renderImageObj mainSurf (gameOverObj gameargs) 
--           renderImage (gameover imageset) mainSurf 0 0 349 57 255 0x00000000

createBlockObj:: Int -> Int -> BlockType -> State GameArgs BlockObj
createBlockObj x y blocktype = do
  gameargs <- get
  let
      imageset = imageSet gameargs
      image
          | blocktype == BlockA = blockA imageset
          | blocktype == BlockB = blockB imageset
          | blocktype == BlockC = blockC imageset
          | blocktype == BlockG = blockG imageset
      imageobj = ImageObj (fromIntegral $ defaultBlockImgPosX x)
                           (fromIntegral $ defaultBlockImgPosY y)
                           fieldBlockSize fieldBlockSize 255 0x00000000 image
  
  return $ BlockObj BS_Stay blocktype imageobj

createRandomBlockObj:: Int -> Int -> State GameArgs BlockObj
createRandomBlockObj x y = do
  bt <- nextBlockType
  createBlockObj x y bt
  where
    nextBlockType:: State GameArgs BlockType
    nextBlockType = do
      gameargs <- get
      let (num, newgen) =  randomR (0, 3) $ stdGen gameargs
      put gameargs{stdGen = newgen}
      return $ putBlock num  

    putBlock:: Int -> BlockType
    putBlock 0 = BlockA
    putBlock 1 = BlockB
    putBlock 2 = BlockC
    putBlock 3 = BlockG
    putBlock _ = BlockG

initGame:: State GameArgs ()
initGame = do
  initScene
  initField
           
initScene:: State GameArgs ()
initScene = do
  appendScene newGameMainScene              
                 
initField:: State GameArgs ()
initField = do
  blockList <- sequence [createRandomBlockObj x y 
                             | x<-[0..(fieldBlockMaxX-1)], y<-[fieldBlockMaxY..(fieldBlockMaxY*2-1)]]
  putFieldBlock $ Util.splitEvery fieldBlockMaxX blockList

renderFiledBlock:: Surface -> FieldBlock -> IO Bool
renderFiledBlock mainSurf fieldblock =
  fmap and $ mapM (\b -> renderImageObj mainSurf (imageObj b)) $ join fieldblock

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

getFieldBlock:: State GameArgs FieldBlock
getFieldBlock = do
  gameargs <- get
  return $ fieldBlock gameargs

putFieldBlock:: FieldBlock -> State GameArgs ()
putFieldBlock fb =
  modify (\gameargs ->
              gameargs{fieldBlock = fb})
    
modifyFieldBlock:: (FieldBlock -> FieldBlock) -> State GameArgs ()
modifyFieldBlock func = do
  modify (\gameargs ->
              gameargs{fieldBlock = func $ fieldBlock gameargs})

setBlockRotateState:: (Int, Int) -> FieldBlock -> FieldBlock
setBlockRotateState (x, y) fb = 
    swapBlock x y (x+1) y $
    changeBlock  x    y (\obj -> obj{blockstate = BS_RotateRight}) $
    changeBlock (x+1) y (\obj -> obj{blockstate = BS_RotateLeft}) fb
               
appendBlock:: State GameArgs () -- FieldBlock -> StdGen -> ImageSet -> (FieldBlock, StdGen)
appendBlock = do
  fb  <- getFieldBlock
  fb' <- zipWithM putBlock [0..] fb
  putFieldBlock fb'
  where
    putBlock:: Int -> [BlockObj] -> State GameArgs [BlockObj]
    putBlock x list = do
      let len = length list
      let newBlockNum = fieldBlockMaxY - len
      al <- mapM (\y -> createRandomBlockObj x (newBlockNum + y)) 
                 [len..(fieldBlockMaxY-1)]
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

removeBlock:: FieldBlock -> FieldBlock
removeBlock fb =
    map (filter (\x -> (blockstate x) /= BS_Removed)) $ mapFieldObj decAplha fb
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


createEraseList:: FieldBlock -> [(Int, Int)]
createEraseList fb =
    nub $ foldl baseblock []
            [(x,y) | x<-[0..fieldBlockMaxX], y<-[0..fieldBlockMaxY]]

    where
      baseblock:: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
      baseblock eraselist pos 
        | getBlockType fb (fst pos) (snd pos) == BlockG
            = eraselist
        | otherwise = 
            foldl (eraseline pos) eraselist [(1,0), (0,1)]

      eraseline:: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
      eraseline (x, y) eraselist (dx, dy) =
          let
              len = sameBlockNum fb x y dx dy
          in
            if len >= 4 then
                foldl (\el n -> (x+n*dx,y+n*dy):el) eraselist [0..len-1]
            else
                eraselist

                
setRemoveState:: State GameArgs Int
setRemoveState = do
  fb <- getFieldBlock
  let eraselist  = createEraseList fb
  modifyFieldBlock $ changeBlockState eraselist
  return $ length eraselist

  where
    changeBlockState:: [(Int, Int)] -> FieldBlock -> FieldBlock
    changeBlockState eraselist fb =
        foldl (\fl (x, y) -> changeAroundBlockState (x, y) $
                             changeBlock x y (\f -> f {blockstate = BS_Removing}) fl)
                    fb eraselist

    changeAroundBlockState:: (Int, Int) -> FieldBlock -> FieldBlock
    changeAroundBlockState (x, y) fb =
        cbt (x, y-1) $ cbt (x, y+1) $ cbt (x+1, y) $ cbt (x-1, y) fb
        where
          cbt (x, y) f
              | 0 <= x && x < fieldBlockMaxX &&
                0 <= y && y < fieldBlockMaxY &&
                getBlockType f x y == BlockG =
                  changeBlock x y (\t -> t {blockstate = BS_Removing}) f
              | otherwise =
                  f
            
sameBlockNum::  FieldBlock -> Int -> Int -> Int -> Int -> Int
sameBlockNum fieldBlock x y dx dy =
  sameBlockNum' (x+dx) (y+dy) (getBlockType fieldBlock x y)
    where
      sameBlockNum':: Int -> Int -> BlockType -> Int
      sameBlockNum' x' y' b
        | 0<=x && x<fieldBlockMaxX &&
          0<=y && y<fieldBlockMaxY &&
          b == (getBlockType fieldBlock x' y') =
            sameBlockNum' (x'+dx) (y'+dy) b
        | otherwise =
            (abs (x'-x)) + (abs (y'-y))

changeBlock:: Int -> Int -> (BlockObj -> BlockObj) -> FieldBlock -> FieldBlock
changeBlock x y func fieldBlock =
    let
        (left, (item:right)) = splitAt y (fieldBlock!!x)
    in
        replaceItem x fieldBlock (left ++ [(func item)] ++ right)

--renumberBlock:: FieldBlock  -> FieldBlock
--renumberBlock =
--    mapFieldObj (\x y obj -> obj {posX = x, posY = y})

getBlock:: FieldBlock -> Int -> Int -> BlockObj
getBlock fieldBlock x y =
     fieldBlock!!x!!y

getBlockType:: FieldBlock -> Int -> Int -> BlockType
getBlockType fieldBlock x y
    | 0<=x && x<fieldBlockMaxX &&
      0<=y && y<fieldBlockMaxY =
          blocktype (fieldBlock !! x !! y)
    | otherwise =
        BlockWall
               
setBlock:: Int -> Int -> BlockObj -> FieldBlock -> FieldBlock
setBlock x y obj fieldBlock =
    changeBlock x y (\_ -> obj) fieldBlock

swapBlock:: Int -> Int -> Int -> Int -> FieldBlock -> FieldBlock
swapBlock ax ay bx by fieldBlock =
    let
        a = getBlock fieldBlock ax ay 
        b = getBlock fieldBlock bx by
    in
        setBlock bx by a $ setBlock ax ay b fieldBlock

unsetBlock:: Int -> Int -> FieldBlock -> FieldBlock
unsetBlock x y fieldBlock =
    replaceItem x fieldBlock (removeItem y (fieldBlock!!x))

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

anyFieldObjState:: BlockState -> FieldBlock -> Bool
anyFieldObjState bs fb = 
    anyFieldObj (\obj -> (blockstate obj) == bs) fb
                  
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

decLifeTime:: Integer -> State GameArgs ()
decLifeTime n = do
  gameargs <- get
  let nlt = max 0 $ (lifeTime gameargs) - n
  put gameargs{lifeTime = nlt}
  when(nlt == 0) $ do
       appendScene $ newGameOverScene $ imageSet gameargs
       setGameState GS_GameOver

--setBlockImgPos:: BlockObj -> BlockObj
--setBlockImgPos obj =
--    let
--        imgX = 50 + (posX obj)*32
--        imgY = 50 + (fieldBlockMaxY-(posY obj))*32
--    in
--        obj {imageObj = ((imageObj obj) {x = imgX, y = imgY})}

setLevel:: State GameArgs ()
setLevel =
    modify $ \gameargs -> gameargs{level = eraseCnt gameargs  `divint` 10 + 1}

setGameState:: GameState -> State GameArgs ()
setGameState gs =
    modify $ \gameargs -> gameargs{gameState = gs}

incEraseCnt:: State GameArgs ()
incEraseCnt =
    modify $ \gameargs -> gameargs{eraseCnt = (eraseCnt gameargs) + 1}
                          
