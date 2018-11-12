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
getRandomMinePositions 0 0 _ = []
getRandomMinePositions _ _ 0 = []
getRandomMinePositions width height numberOfMines generator =
    let length = width * height
        shuffledBoard = shuffle' [(w, h) | w <- [0..width], h <- [0..height]] length generator
        mines = take numberOfMines shuffledBoard
    in mines
-- TODO implement random position generation correctly
--getRandomMinePositions width height prob = [(0, 1),(4, 0),(11, 12),(7, 0)]
