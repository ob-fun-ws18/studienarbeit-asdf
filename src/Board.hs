module Board (Board(..), Position(..), FieldContent(..), FieldState(..), NeighbourCount(..), isFlagged) where



type Position = (Int, Int)

data NeighbourCount = Nil | One | Two | Three | Four | Five | Six | Seven | Eight

instance Show NeighbourCount where
    show Nil = "0"
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

data FieldState = Revealed | Hidden Bool deriving Show

isFlagged :: FieldState -> Bool
isFlagged (Hidden flagged) = flagged

data Board = Board
    {
      width :: Int,
      height :: Int,
      fields :: [(Position, FieldContent, FieldState)]
    }

instance Show Board where
    show (Board _ _ fields) = show $ (map second fields)


second :: (Position, FieldContent, FieldState) -> FieldContent
second (_, s, _) = s