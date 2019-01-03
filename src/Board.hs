module Board (
    Board(..),
    Field(..),
    FieldContent(..),
    FieldState(..),
    NeighbourCount(..),
    GameState(..),
    board,
--    revealField,
    flagField, getFieldContent, isGameLost, revealBoard, isGameWon, checkedRevealField, revealRecursive, getFieldFromBoard, getCrossNeighbourCellPositions
    ) where

import Lib (getRandomMinePositions, getSurroundingPositions, setIndex)
import System.Random

data NeighbourCount = Nil | One | Two | Three | Four | Five | Six | Seven | Eight deriving Enum

instance Show NeighbourCount where
    show Nil = "0"
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
    (NoMine Nil) == (NoMine Nil) = True
    (NoMine One) == (NoMine One) = True
    (NoMine Two) == (NoMine Two) = True
    (NoMine Three) == (NoMine Three) = True
    (NoMine Four) == (NoMine Four) = True
    (NoMine Five) == (NoMine Five) = True
    (NoMine Six) == (NoMine Six) = True
    (NoMine Seven) == (NoMine Seven) = True
    (NoMine Eight) == (NoMine Eight) = True
    _ == _ = False

data GameState = GameWon | GameLost | GameNotFinished

instance Eq GameState where
    GameWon == GameWon = True
    GameLost == GameLost = True
    GameNotFinished == GameNotFinished = True
    _ == _ = False

instance Show GameState where
    show (GameWon) = "GameWon"
    show (GameLost) = "GameLost"
    show (GameNotFinished) = "GameNotFinished"

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
      fields :: [Field],
      gameState:: GameState
    }

instance Show Board where
    show (Board width _ _ fields _) = (\row -> (foldl (\acc y -> acc ++ (show y) ++ "\n") "" row)) (splitEvery width fields)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

board :: Int -> Int -> Int -> Int -> Board

-- create randomGenerator with gen <- newStdGen
board width height numberOfMines seed = do
    let minesPos = getRandomMinePositions width height numberOfMines seed
    let positions = [(w, h) | w <- [0..width-1], h <- [0..height-1]]
    let boardContent = map (\pos -> getFieldContent pos minesPos width height) positions
    Board width height numberOfMines (map (\content -> Field content (Hidden False)) boardContent) GameNotFinished


getFieldContent :: (Int, Int) -> [(Int, Int)] -> Int -> Int -> FieldContent
getFieldContent pos minesPos width height = do

    let countSurroundingMines xs ys = length [x | x <- xs, x `elem` ys]
        surroundingPos = getSurroundingPositions pos width height
        numSurroundingMines = countSurroundingMines surroundingPos minesPos
    if (pos `elem` minesPos)
        then Mine
    else
        NoMine (toEnum numSurroundingMines)

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
        Board (width board) (height board) (numberOfMines board) newFields GameNotFinished

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
revealBoard board 0 = checkedRevealField board 0 0
revealBoard board index = do
    let x = mod index (width board)
        y = quot index (height board)
    revealBoard (checkedRevealField board x y) (index - 1)

checkedRevealField :: Board -> Int -> Int -> Board
checkedRevealField board x y = do
    let boardNext = revealRecursive (revealField board x y) [(x, y)]
    if (content $ getFieldFromBoard board x y) == Mine then
        boardNext { gameState = GameLost }
    else if (isGameWon boardNext) then
        boardNext { gameState = GameWon }
    else
        boardNext

isInBounds :: Board -> Int -> Int -> Bool
isInBounds b x y = do
      let w = width b
          h = height b
      if (x < 0 || y < 0)
        then False
      else if (x >= w || y >= h)
        then False
      else True

getCrossNeighbourCellPositions :: Board -> Int -> Int -> [(Int, Int)]
getCrossNeighbourCellPositions b x y = do
      filter (\(x1, y1) -> isInBounds b x1 y1) [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]

isHidden :: Board -> Int -> Int -> Bool
isHidden b x y = Revealed /= (state $ getFieldFromBoard b x y)

getHiddenCrossNeighbourPositions :: Board -> Int -> Int -> [(Int, Int)]
getHiddenCrossNeighbourPositions b x y = filter (\(x1, y1) -> isHidden b x1 y1) (getCrossNeighbourCellPositions b x y)

revealField :: Board -> Int -> Int -> Board
revealField b x y = do
      let f = getFieldFromBoard b x y
          pos = ((width b) * x) + y
          newFields = setIndex (fields b) pos (f { state = Revealed })
      b { fields = newFields }

tryReveal :: Board -> Int -> Int -> Board
tryReveal b x y = do
      let c = content $ getFieldFromBoard b x y
      case c of
        Mine -> b
        _ -> revealField b x y

revealRecursive :: Board -> [(Int, Int)] -> Board
revealRecursive b [] = b
revealRecursive b ((x, y):rest) = do
      let c = content $ getFieldFromBoard b x y
      case c of
        Mine -> revealRecursive b rest
        NoMine Nil -> do
          let b2 = tryReveal b x y
              b3 = revealRecursive b2 rest
          revealRecursive b3 (getHiddenCrossNeighbourPositions b3 x y)
        _ -> revealRecursive (tryReveal b x y) rest
