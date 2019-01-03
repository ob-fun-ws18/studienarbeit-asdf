module Gui(guiMain) where
import Control.Monad
import Data.IORef

import Text.Printf
import Board
import Lib
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Main entry point.
guiMain = startGUI defaultConfig { jsStatic = Just "." } setup



setup :: Window -> UI ()
setup view = do
    return view # set title "Element Test"

    inputWidth <- UI.input # set (attr "placeholder") "width"
    inputHeight <- UI.input # set (attr "placeholder") "height"
    inputMines <- UI.input # set (attr "placeholder") "mines"
    startGameBtn <- UI.button   # set UI.text "start"

    field <- UI.div #. "Board"

    getBody view #+ [element inputWidth, element inputHeight, element inputMines, element startGameBtn]


    on UI.click startGameBtn $ \_ -> do

        counter <- UI.div   # set UI.text "0"
        count <- liftIO $ newIORef 0

-- TODO
--         let width = get value inputWidth
--         let height = get value inputHeight
--         let mines = get value inputMines

        let widthInput = 10
            heightInput = 10
            numberOfMines = 10

        boardRef <- liftIO $ newIORef (board widthInput heightInput numberOfMines 1 )

        let mkString s = UI.string s # set UI.class_ "string"
        let nFields = [0..(widthInput * heightInput) - 1]


        getBody view #+ [UI.tr #+
            [
            do
                let pos = h + w * heightInput
                button <- UI.button
                  # set UI.class_ "minesweeper fields"
                  # set UI.type_ "button"
                  #+ [string "_"]
                  # set UI.id_ ("field_" ++ (show pos))

                on UI.click button $ \_ -> do
                    --element button4 # set UI.text "not found"

                    rBoard <- liftIO $ readIORef boardRef
                    liftIO $ writeIORef boardRef (checkedRevealField rBoard w h)
                    rBoard <- liftIO $ readIORef boardRef

                    element button # set UI.text (show (fields rBoard !! pos))
                    --let asdf = showNewBoard rBoard view
                    let state = gameState rBoard
                        revealedBoard = revealBoard rBoard (width rBoard * height rBoard)
                    if state == GameLost then do
                        liftIO $ putStrLn (show revealedBoard)
                        --error "Game over, you dun goofed"
                    else if state == GameWon then do
                        --liftIO $ putStrLn (show revealedBoard)
                        error "Game over, you won"
                    else do
                        liftIO $ putStrLn (show rBoard)
                    return ()
                return (button)
            | w <- [0..widthInput - 1]]
            | h <- [0..heightInput - 1]]


-- showNewBoard :: Board -> Window -> [UI Element]
-- showNewBoard b uiView = do
--     map (\fieldNum -> showFieldOnGUI fieldNum uiView) [0..(height b) * (width b)]
--
-- showFieldOnGUI :: Board -> Int -> Window -> UI (Element)
-- showFieldOnGUI gameBoard fieldNum uiView = do
--     btn <- getElementById uiView "field_" (show fieldNum)
--     case btn of
--         Nothing -> element btn # set UI.text "not found" -- !!!111!1!!1
--         Just btn -> element btn # set UI.text (show (fields gameBoard !! fieldNum))

-- TODO update whole board instead of each button on its own (something like showNewBoard)
-- TODO parse inputs (width, height, mines)
-- TODO images for buttons ?
