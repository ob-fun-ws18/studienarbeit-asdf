{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import Data.ByteString.Char8
import System.Random

import  Board (board)


main :: IO ()
main = quickHttpServe site



site :: Snap ()
site =
    ifTop (writeBS "hello minesweeper") <|>
    route [
          --, ("new-game-p/:height/:width/:mines/:randomSeed", newGameHandler)
           ("new-game", newGameDefaultHandler)
          ] <|>
    dir "static" (serveDirectory ".")


newGameDefaultHandler :: Snap ()
newGameDefaultHandler = do
    writeBS $ pack (show (board 10 10 10 (mkStdGen 1)))


-- newGameHandler :: Snap ()
-- newGameHandler = do
--     Just heightParam <- getParam "height"
--     Just widthParam <- getParam "width"
--     Just minesParam <- getParam "mines"
--     Just randomSeedParam <- getParam "randomSeed"
--
--     -- TODO parse this somehow xD
--     let height = readInt heightParam
--     let width = readInt widthParam
--     let mines = readInt minesParam
--     let randomSeed = readInt randomSeedParam
--
--
--     let boardRepresentation = show (board height width mines (mkStdGen randomSeed))
--     maybe (writeBS "must specify params in URL")
--         writeBS (pack boardRepresentation)