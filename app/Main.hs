module Main where

import Gui (guiMain)
import Lib
--import System.Random
import Board
import Lib
main :: IO  ()
main = guiMain
--
-- main :: IO ()
-- main = do
--     putStrLn "Please enter the width of the board"
--     width <- readLn :: IO Int
--     putStrLn "Please enter the height of the board"
--     height <- readLn :: IO Int
--     putStrLn "Please enter the amount of mines"
--     mines <- readLn :: IO Int
--     --gen <- newStdGen
--     playGame (board width height mines 1)
--
-- playGame :: Board -> IO ()
-- playGame b = do
--     putStrLn (show b)
--     putStrLn "--------------"
--     putStrLn "Enter a command"
--     command <- getLine
--     let ws = words command
--     if (length ws /= 3)
--       then error ("expected 3 args but got " ++ show (length ws))
--     else do
--       let instruction = ws!!0
--           w = read (ws!!1) :: Int
--           h = read (ws!!2) :: Int
--       let board = execute b instruction w h
--
--       let state = gameState board
--       let revealedBoard = revealBoard board (width board * height board)
--       if state == GameLost then do
--         putStrLn (show revealedBoard)
--         error "Game over, you dun goofed"
--       else if state == GameWon then do
--         putStrLn (show revealedBoard)
--         error "Game over, you won"
--       else do
--         playGame board
--
-- execute :: Board -> String -> Int -> Int -> Board
-- execute b instruction width height
--     | instruction == "reveal" = checkedRevealField b width height
--     | instruction == "flag"   = flagField b width height
--     | otherwise               = error "unknown command"
