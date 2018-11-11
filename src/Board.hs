module Board (Board(..), FieldContent(..), FieldState(..), NeighbourCount(..), board) where

import Lib (getRandomMinePositions)

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

board :: Int -> Int -> Int -> Board
board width height prob = do
    let minesPos = getRandomMinePositions width height prob
    let positions = [(w, h) | w <- [0..width], h <- [0..height]]
    let boardContent = map (\pos -> getFieldContent pos minesPos) positions
    Board width height (map (\content -> Field content Revealed) boardContent)

getFieldContent :: (Int, Int) -> [(Int, Int)] -> FieldContent
getFieldContent pos minePos =
    -- TODO calculate NeighbourCount correctly
    if (pos `elem` minePos) then Mine else NoMine One
