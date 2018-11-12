module Board (Board(..), FieldContent(..), FieldState(..), NeighbourCount(..), board) where

import Lib (getRandomMinePositions, getSurroundingPositions)
import System.Random

data NeighbourCount = Nil | One | Two | Three | Four | Five | Six | Seven | Eight

instance Show NeighbourCount where
    show Nil = " "
    show One = "1"
    show Two = "2"
    show Three = "3"
    show Five  = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"

data FieldContent = Mine | NoMine NeighbourCount

instance Show FieldContent where
    show (Mine) = "*"
    show (NoMine c) = show c

data FieldState = Revealed | Hidden deriving (Show, Eq)
data Field = Field
  {
    content :: FieldContent,
    state :: FieldState
  }

instance Show Field where
    show (Field c s) = if (s == Hidden) then "_" else show c

data Board = Board
    {
      width :: Int,
      height :: Int,
      fields :: [Field]
    } deriving Show

board :: Int -> Int -> Int -> StdGen -> Board

-- create randomGenerator with gen <- newStdGen
board width height numberOfMines randomGenerator = do
    let minesPos = getRandomMinePositions width height numberOfMines randomGenerator
    let positions = [(w, h) | w <- [0..width], h <- [0..height]]
    let boardContent = map (\pos -> getFieldContent pos minesPos width height) positions
    Board width height (map (\content -> Field content Revealed) boardContent)


getFieldContent :: (Int, Int) -> [(Int, Int)] -> Int -> Int -> FieldContent
getFieldContent pos minesPos width height = do

    let countSurroundingMines xs ys = length [x | x <- xs, x `elem` ys]
        surroundingPos = getSurroundingPositions pos width height
        numSurroundingMines = countSurroundingMines surroundingPos minesPos

    if (pos `elem` minesPos)
        then Mine
    else
    -- TODO change this ugly block (too lazy to google right now)
        if numSurroundingMines == 0
            then NoMine Nil
        else if numSurroundingMines == 1
            then NoMine One
        else if numSurroundingMines == 2
            then NoMine Two
        else if numSurroundingMines == 3
            then NoMine Three
        else if numSurroundingMines == 4
            then NoMine Four
        else if numSurroundingMines == 5
            then NoMine Five
        else if numSurroundingMines == 6
            then NoMine Six
        else if numSurroundingMines == 7
            then NoMine Seven
        else if numSurroundingMines == 8
            then NoMine Eight
        else
            -- todo add exception
            NoMine Nil
