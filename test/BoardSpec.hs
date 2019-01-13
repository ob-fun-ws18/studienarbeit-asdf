{-# LANGUAGE ScopedTypeVariables #-}
module BoardSpec (spec) where

import Board
import Test.Hspec

spec :: Spec
spec =
    context "Baord.hs tests" $ do
      describe "board" $ do
        it "is a 1x1 board with 1 mine " $ do
          (content $ (fields $ board 1 1 1 1)!!0) `shouldBe` Mine
      describe "getFieldContent" $ do
        it "should be mine" $ do
          (getFieldContent (0,0) [(0,0)] 1 1) `shouldBe` Mine
        it "should be NoMine Nil" $ do
          (getFieldContent (0,0) [] 1 1) `shouldBe` (NoMine Nil)
      describe "flagField" $ do
        it "should flag Hidden field" $ do
          let b = board 1 1 1 1
              s = state ((getFieldFromBoard (flagField b 0 0)) 0 0)
          s `shouldBe` (Hidden True)
        it "should not flag Revealed field" $ do
          let b = revealField (board 1 1 1 1) 0 0
              b2 = flagField b 0 0
          b `shouldBe` b2
      describe "getFieldFromBoard" $ do
        it "should return field" $
          (getFieldFromBoard (board 1 1 1 1) 0 0) `shouldBe` (Field Mine (Hidden False))
      describe "isGameLost" $ do
        it "should be lost" $ do
          let b = revealField (board 1 1 1 1) 0 0
          (isGameLost b 0 0) `shouldBe` True
        it "shoud not be lost" $
          (isGameLost (board 1 1 0 1) 0 0) `shouldBe` False
