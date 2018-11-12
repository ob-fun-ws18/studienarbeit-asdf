module Lib where

import System.Random
import System.Random.Shuffle

setIndex
    :: [a] -- the list
    -> Int -- index
    -> a -- new value
    -> [a] -- the new list
setIndex xs ii v
    | ii < 0 = error "Bad index"
    | otherwise = _setIndex xs ii v
    where
        _setIndex [] _ _ = error "Bad index"
        _setIndex (_ : xs) 0 v = v : xs
        _setIndex (x : xs) ii v = x : (setIndex xs (ii - 1) v)

setIndex2
    :: [[a]]  -- the list
    -> Int    -- row
    -> Int    -- column
    -> a      -- new value
    -> [[a]]  -- the new list
setIndex2 list row col value =
    let
        (head, (mid : tail)) = splitAt row list
        newSubList = setIndex mid col value
    in  head ++ (newSubList : tail)

getRandomMinePositions
    :: Int -- width
    -> Int -- height
    -> Int -- number of mines
    -> StdGen -- random number generator
    -> [(Int, Int)] -- mine index's
getRandomMinePositions 0 0 _ _ = []
getRandomMinePositions _ _ 0 _ = []
getRandomMinePositions width height numberOfMines generator =
    let length = width * height
        shuffledBoard = shuffle' [(w, h) | w <- [0..width-1], h <- [0..height-1]] length generator
        mines = take numberOfMines shuffledBoard
    in mines

getSurroundingPositions
    :: (Int, Int) -- position
    -> Int -- width
    -> Int -- height
    -> [(Int, Int)] -- surrounding positions
getSurroundingPositions position width height =
    let minWidth  = max (fst position - 1) 0
        maxWidth  = min (fst position + 1) width
        minHeight = max (snd position - 1) 0
        maxHeight = min (snd position + 1) height

    in [(w,h) | w <- [minWidth..maxWidth], h <- [minHeight..maxHeight], (w,h) /= position]
