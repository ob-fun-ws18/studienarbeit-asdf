{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec =
    context "Lib.hs tests" $ do
      describe "setIndex" $ do
        it "sets the value at the first index" $
          (setIndex [0, 0, 0] 0 1) `shouldBe` [1, 0, 0]
        it "sets the value at the second index" $
          (setIndex [0, 0, 0] 1 1) `shouldBe` [0, 1, 0]
        it "sets the value at the last index" $
          (setIndex [0, 0, 0] 2 1) `shouldBe` [0, 0, 1]
      describe "getRandomMinePositions" $ do
        it "should return empty list for width = 0" $
          (getRandomMinePositions 0 10 10 1) `shouldBe` []
        it "should return empty list for height = 0" $
          (getRandomMinePositions 10 0 10 1) `shouldBe` []
        it "should return empty list for numberOfMines = 0" $
          (getRandomMinePositions 10 10 0 1) `shouldBe` []
        it "should return list with 10 mines" $
          (length $ getRandomMinePositions 10 10 10 1) `shouldBe` 10
      describe "getSurroundingPositions" $ do
        it "is not close to the boundary so it should return all 8 surrounding positions" $
          (getSurroundingPositions (1, 1) 10 10) `shouldBe` [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]
        it "is in the corner so it should return only the 3 surrounding positions" $
          (getSurroundingPositions (0, 0) 10 10) `shouldBe` [(0,1),(1,0),(1,1)]
        it "is close to left boundary so it should return only the 3 surrounding positions" $
          (getSurroundingPositions (0, 1) 10 10) `shouldBe` [(0,0),(0,2),(1,0),(1,1),(1,2)]
        it "is close to upper boundary so it should return only the 3 surrounding positions" $
          (getSurroundingPositions (1, 0) 10 10) `shouldBe` [(0,0),(0,1),(1,1),(2,0),(2,1)]
