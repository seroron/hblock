module UtilSpec(spec) where

import Test.Hspec
import Util
    
spec :: Spec
spec = do
  describe "mapAccumFilterR" $ do
        it "all just" $ do
           Util.mapAccumFilterR (\x y -> (x+1, Just $ y*10)) 10 [1,2,3] `shouldBe` (13,[10,20,30])
        it "one just" $ do
           Util.mapAccumFilterR (\x y -> (x+1, if y==2 then (Just $ y*10) else Nothing)) 10 [1,2,3] `shouldBe` (13,[20]) 
        it "no just" $ do
           Util.mapAccumFilterR (\x y -> (x+1, if y==2 then (Just $ y*10) else Nothing)) 10 [5,6,7] `shouldBe` (13,[]) 
        it "empty input" $ do
           Util.mapAccumFilterR (\x y -> (x+1, if y==2 then (Just $ y*10) else Nothing)) 10 [] `shouldBe` (10, [])
                         
