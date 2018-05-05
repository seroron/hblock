module HBlockSpec(spec) where

import Test.Hspec
import System.IO
import System.Directory
import HBlock

spec :: Spec
spec = do
  describe "loadHiScore" $ do
        it "load saved data" $ do
          saveHiScore 120
          s <- loadHiScore 
          s `shouldBe` 120 

        it "load -100" $ do
          saveHiScore (-100)
          s <- loadHiScore 
          s `shouldBe` 0 
            
        it "empty file" $ do
          withFile "save.txt" WriteMode $ \h -> hPutStrLn h ""
          s <- loadHiScore 
          s `shouldBe` 0 

        it "invalid string" $ do
          withFile "save.txt" WriteMode $ \h -> hPutStrLn h "abc"
          s <- loadHiScore 
          s `shouldBe` 0

        it "no such file" $ do
          removeFile "save.txt"
          s <- loadHiScore 
          s `shouldBe` 0 

