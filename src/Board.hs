module Board (
    Board(..),
    Field(..),
    FieldContent(..),
    FieldState(..),
    NeighbourCount(..),
    board,
    revealField
    ) where

import Lib (getRandomMinePositions, getSurroundingPositions, setIndex)
import System.Random

data NeighbourCount = Nil | One | Two | Three | Four | Five | Six | Seven | Eight deriving Enum

instance Show NeighbourCount where
    show Nil = " "
    show One = "1"
    show Two = "2"
    show Three = "3"
    show Four = "4"
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
    show (Field _ Hidden) = "_"
    show (Field c Revealed) = show c

data Board = Board
    {
      width :: Int,
      height :: Int,
      fields :: [Field]
    }

instance Show Board where
    show (Board width _ fields) = (\row -> (foldl (\acc y -> acc ++ (show y) ++ "\n") "" row)) (splitEvery width fields)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

board :: Int -> Int -> Int -> StdGen -> Board

-- create randomGenerator with gen <- newStdGen
board width height numberOfMines randomGenerator = do
    let minesPos = getRandomMinePositions width height numberOfMines randomGenerator
    let positions = [(w, h) | w <- [0..width-1], h <- [0..height-1]]
    let boardContent = map (\pos -> getFieldContent pos minesPos width height) positions
    Board width height (map (\content -> Field content Hidden) boardContent)


getFieldContent :: (Int, Int) -> [(Int, Int)] -> Int -> Int -> FieldContent
getFieldContent pos minesPos width height = do

    let countSurroundingMines xs ys = length [x | x <- xs, x `elem` ys]
        surroundingPos = getSurroundingPositions pos width height
        numSurroundingMines = countSurroundingMines surroundingPos minesPos

    if (pos `elem` minesPos)
        then Mine
    else
        NoMine (toEnum numSurroundingMines)


revealField :: Board -> Int -> Int -> Board
revealField board x y = do
    let pos = ((width board) * x) + y
    let field = (fields board)!!pos
    let revealedField = Field (content field) Revealed
    let newFields = setIndex (fields board) pos revealedField
    Board (width board) (height board) newFields
