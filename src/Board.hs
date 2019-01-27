-- | Module for the Board and all its components.
module Board (
    Board(..),
    Field(..),
    FieldContent(..),
    FieldState(..),
    NeighbourCount(..),
    GameState(..),
    board,
    flagField,
    getFieldContent,
    isGameLost,
    revealBoard,
    revealField,
    isGameWon,
    checkedRevealField,
    revealRecursive,
    getFieldFromBoard,
    getCrossNeighbourCellPositions
    ) where

import Lib (getRandomMinePositions, getSurroundingPositions, setIndex)
--import System.Random

data NeighbourCount = Nil | One | Two | Three | Four | Five | Six | Seven | Eight deriving Enum

instance Show NeighbourCount where
    show Nil = ""
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
  } deriving (Eq)

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
    } deriving (Eq)

instance Show Board where
    show (Board width _ _ fields _) = (\row -> (foldl (\acc y -> acc ++ (show y) ++ "\n") "" row)) (splitEvery width fields)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list


-- | Create a new Board.
board
    :: Int -- ^ the width
    -> Int -- ^ the height
    -> Int -- ^ the number of mines
    -> Int -- ^ the seed
    -> Board -- ^ the new Board.
board width height numberOfMines seed = do
    let minesPos = getRandomMinePositions width height numberOfMines seed
    let positions = [(w, h) | w <- [0..width-1], h <- [0..height-1]]
    let boardContent = map (\pos -> getFieldContent pos minesPos width height) positions
    Board width height numberOfMines (map (\content -> Field content (Hidden False)) boardContent) GameNotFinished

-- | Calculates the number of mines surrounding the field at the given coordinate and determines its content.
getFieldContent
    :: (Int, Int) -- ^ the coordinate
    -> [(Int, Int)] -- ^ the coordinates of the mines
    -> Int -- ^ the width of the board
    -> Int -- ^ the height of the board
    -> FieldContent -- the field content (e.g. Mine, NoMine Nil, NoMine One, ...)
getFieldContent pos minesPos width height = do
    let countSurroundingMines xs ys = length [x | x <- xs, x `elem` ys]
        surroundingPos = getSurroundingPositions pos width height
        numSurroundingMines = countSurroundingMines surroundingPos minesPos
    if (pos `elem` minesPos)
        then Mine
    else
        NoMine (toEnum numSurroundingMines)

-- | Flags the field on the given coordinate.
flagField
    :: Board -- ^ receiver
    -> Int -- ^ x coordinate
    -> Int -- ^ y coordinate
    -> Board -- ^ the new board
flagField board x y = do
    let pos = ((width board) * x) + y
    let field = getFieldFromBoard board x y
    if ((state field) == Revealed)
      then board
      else do
        let setFlag =  Hidden False == state field
        let flaggedField = Field (content field) (Hidden setFlag)
        let newFields = setIndex (fields board) pos flaggedField
        Board (width board) (height board) (numberOfMines board) newFields GameNotFinished

-- | Returns the field on the given coordinate.
getFieldFromBoard
    :: Board -- ^ receiver
    -> Int -- ^ x coordinate
    -> Int -- ^ y coordinate
    -> Field -- ^ the field
getFieldFromBoard board x y = do
    let pos = ((width board) * x) + y
    (fields board)!!pos

-- | Tells if a field that is being reveald is a mine and therefore the loosing condtiion is met.
isGameLost
    :: Board -- ^ receiver
    -> Int  -- ^ x coordinate
    -> Int -- ^ y coordinate
    -> Bool -- ^ is game lost
isGameLost board x y = do
    let field = getFieldFromBoard board x y
    Mine == content field

-- | Tells if the board is solved and therefore the winning condition is met.
isGameWon
    :: Board -- ^ receiver
    -> Bool -- ^ is game won
isGameWon board = do
     let isFieldHidden = map (\field -> if state field == Revealed then 0 else 1) (fields board)
         numberOfHiddenFields = foldr (+) 0 isFieldHidden
     numberOfMines board == numberOfHiddenFields


-- | Reveals a field by index like checkedRevealField.
revealBoard
    :: Board -- ^ the receiver
    -> Int -- ^ the index
    -> Board -- ^ the changed board
revealBoard board 0 = checkedRevealField board 0 0
revealBoard board index = do
    let x = mod index (width board)
        y = quot index (height board)
    revealBoard (checkedRevealField board x y) (index - 1)

-- | Reveals a field and recursively reveals all nil fields and the first non-nil field except mines.
-- If the selected field is a mine the game state will be changed to lost.
-- If there are no more fields to reveal but mines the game state will be changed to won.
-- Else the game state stays at not finished.
checkedRevealField
    :: Board -- ^ the receiver
    -> Int -- ^ x coordinate
    -> Int -- ^ y coordinate
    -> Board -- ^ the changed Board
checkedRevealField board x y = do
    let boardNext = revealRecursive (revealField board x y) [(x, y)]
    if (content $ getFieldFromBoard board x y) == Mine then
        boardNext { gameState = GameLost }
    else if (isGameWon boardNext) then
        boardNext { gameState = GameWon }
    else
        boardNext


-- | Tells if a position is inside the boundary of the board.
isInBounds
    :: Board -- ^ receiver
    -> Int -- ^ x coordinate
    -> Int -- ^ y coordinate
    -> Bool -- ^ coordinate is on board
isInBounds b x y = do
      let w = width b
          h = height b
      if (x < 0 || y < 0)
        then False
      else if (x >= w || y >= h)
        then False
      else True

-- | Returns the coordinates that are above, below, left and right from the given coordinate.
-- Coordinates that are not within the boundary will be filtered out.
getCrossNeighbourCellPositions
      :: Board -- ^ receiver
      -> Int -- ^ x coordinate
      -> Int -- ^ y coordinate
      -> [(Int, Int)] -- ^ coordinates of neighbouring cells that are within the boundary
getCrossNeighbourCellPositions b x y = do
      filter (\(x1, y1) -> isInBounds b x1 y1) [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]

-- | Tells if the state of the field with the coordinate x y is Hidden.
isHidden
      :: Board -- ^ receiver
      -> Int -- ^ x coordinate
      -> Int -- ^ y coordinate
      -> Bool -- ^ if Hidden true else false
isHidden b x y = Revealed /= (state $ getFieldFromBoard b x y)

getHiddenCrossNeighbourPositions :: Board -> Int -> Int -> [(Int, Int)]
getHiddenCrossNeighbourPositions b x y = filter (\(x1, y1) -> isHidden b x1 y1) (getCrossNeighbourCellPositions b x y)

-- | Sets the state of the field on the board with the coordinates of x and y to revealed.
revealField
      :: Board -- ^ receiver
      -> Int -- ^ x coordinate
      -> Int -- ^ y coordinate
      -> Board -- ^ new board
revealField b x y = do
      let f = getFieldFromBoard b x y
          pos = ((width b) * x) + y
          newFields = setIndex (fields b) pos (f { state = Revealed })
      b { fields = newFields }

-- | Sets the state of the field on the board with the coordinates of x and y to revealed if it is not a mine.
tryReveal
      :: Board -- ^ receiver
      -> Int -- ^ x coordinate
      -> Int -- ^ y coordinate
      -> Board -- ^ new board
tryReveal b x y = do
      let c = content $ getFieldFromBoard b x y
      case c of
        Mine -> b
        _ -> revealField b x y

-- | Tries to reveal all given coordinates recursivly.
-- If a field is not a mine and also has the value Nil, then it will also
-- recursively reveal the neighbouring fields that are not diagonally located to this field.
revealRecursive
      :: Board -- ^ receiver
      -> [(Int, Int)] -- ^ coordinates to reveal
      -> Board -- ^ the new board
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
