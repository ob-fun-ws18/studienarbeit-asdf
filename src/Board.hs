module Board (
    Board(..),
    Field(..),
    FieldContent(..),
    FieldState(..),
    NeighbourCount(..),
    board,
    revealField,
    flagField, getFieldContent, isGameLost, revealBoard, isGameWon
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

instance Eq FieldContent where
    Mine == Mine = True
    -- TODO
    _ == _ = False

instance Show FieldContent where
    show (Mine) = "*"
    show (NoMine c) = show c

data FieldState = Revealed | Hidden Bool deriving (Show, Eq)
data Field = Field
  {
    content :: FieldContent,
    state :: FieldState
  }

instance Show Field where
    show (Field _ (Hidden False)) = "_"
    show (Field _ (Hidden True)) = "<|"
    show (Field c Revealed) = show c

data Board = Board
    {
      width :: Int,
      height :: Int,
      numberOfMines :: Int,
      fields :: [Field]
    }

instance Show Board where
    show (Board width _ _ fields) = (\row -> (foldl (\acc y -> acc ++ (show y) ++ "\n") "" row)) (splitEvery width fields)

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
    Board width height numberOfMines (map (\content -> Field content (Hidden False)) boardContent)


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
    Board (width board) (height board) (numberOfMines board) newFields


flagField :: Board -> Int -> Int -> Board
flagField board x y = do
    let pos = ((width board) * x) + y
    --let field = (fields board)!!pos
    let field = getFieldFromBoard board x y
    if ((state field) == Revealed)
      then board
      else do
        let flaggedField = Field (content field) (Hidden True)
        let newFields = setIndex (fields board) pos flaggedField
        Board (width board) (height board) (numberOfMines board) newFields

getFieldFromBoard :: Board -> Int -> Int -> Field
getFieldFromBoard board x y = do
    let pos = ((width board) * x) + y
    (fields board)!!pos

isGameLost :: Board -> Int -> Int -> Bool
isGameLost board x y = do
    let field = getFieldFromBoard board x y
    Mine == content field


isGameWon :: Board -> Bool
isGameWon board = do
     let isFieldHidden = map (\field -> if state field == Revealed then 0 else 1) (fields board)
         numberOfHiddenFields = foldr (+) 0 isFieldHidden
     numberOfMines board == numberOfHiddenFields


-- TODO laesst sich bestimmt irgendwie schoen mit foldr schreiben aber ich checks nicht lel
revealBoard :: Board -> Int -> Board
revealBoard board 0 = revealField board 0 0
revealBoard board index = do
    let x = mod index (width board)
        y = quot index (height board)
    revealBoard (revealField board x y) (index - 1)

