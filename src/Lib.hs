module Lib
    (setIndex2, setIndex
    ) where


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