-- | Module for some library functions.
module Lib where

-- | Returns a random number.
getRandomNumber
    :: Int -- ^ the seed
     -> Int -- ^ the range
     -> Int -- ^ the random number
getRandomNumber seed range = ((1103515243 * seed + 123) `mod` (2^16)) `mod` range

-- | Sets the value at the given index.
setIndex
    :: [a] -- ^ the list
    -> Int -- ^ index
    -> a -- ^ new value
    -> [a] -- ^ the new list
setIndex xs ii v
    | ii < 0 = error "Bad index"
    | otherwise = _setIndex xs ii v
    where
        _setIndex [] _ _ = error "Bad index"
        _setIndex (_ : xs) 0 v = v : xs
        _setIndex (x : xs) ii v = x : (setIndex xs (ii - 1) v)

-- | Returns random mine coordinates.
getRandomMinePositions
    :: Int -- ^ width
    -> Int -- ^ height
    -> Int -- ^ number of mines
    -> Int -- ^ seed
    -> [(Int, Int)] -- ^ mine coordinates
getRandomMinePositions 0 _ _ _ = []
getRandomMinePositions _ 0 _ _ = []
getRandomMinePositions _ _ 0 _ = []
getRandomMinePositions width height numberOfMines seed =
    let length = width * height
        mineIndixes = [getRandomNumber (seed * s) length | s <- [0..numberOfMines-1]]
        fullBoard = [(w, h) | w <- [0..width-1], h <- [0..height-1]]
        mines = [fullBoard !! i | i <-mineIndixes]
    in mines

-- | Returns the surrounding positions.
getSurroundingPositions
    :: (Int, Int) -- ^ position
    -> Int -- ^ width
    -> Int -- ^ height
    -> [(Int, Int)] -- ^ surrounding positions
getSurroundingPositions position width height =
    let minWidth  = max (fst position - 1) 0
        maxWidth  = min (fst position + 1) width
        minHeight = max (snd position - 1) 0
        maxHeight = min (snd position + 1) height

    in [(w,h) | w <- [minWidth..maxWidth], h <- [minHeight..maxHeight], (w,h) /= position]
