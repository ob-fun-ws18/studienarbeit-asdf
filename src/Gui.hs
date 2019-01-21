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
    UI.addStyleSheet view "gui-stylesheet.css"
    return view # set title "Haskell Minesweeper"

    caption <- UI.h1 # set UI.text "Haskell Minesweeper"
    inputSideLength <- UI.input # set (attr "placeholder") "side length" # set UI.class_ "gameInput"
    --inputHeight <- UI.input # set (attr "placeholder") "height" # set UI.class_ "gameInput"
    inputMines <- UI.input # set (attr "placeholder") "mines" # set UI.class_ "gameInput"
    startGameBtn <- UI.button   # set UI.text "start" # set UI.class_ "startBtn"
    flagBtn <- UI.button # set UI.text "flag"

    --field <- UI.div #. "Board"
    divInputWrapper <- UI.div #. "inputWrapper" #+ [element caption, element inputSideLength, element inputMines, element startGameBtn, element flagBtn]
    getBody view #+ [element divInputWrapper]
    refIsFlagMode <- liftIO $ newIORef False

    on UI.click flagBtn $ \_ -> do
        isFlagMode <- liftIO $ readIORef refIsFlagMode
        liftIO $ putStrLn (show isFlagMode)
        liftIO $ writeIORef refIsFlagMode (not isFlagMode)

        if isFlagMode then do
            element flagBtn # set UI.text "flag"
        else do
            element flagBtn # set UI.text "reveal"

    on UI.click startGameBtn $ \_ -> do
        element startGameBtn # set UI.enabled False

        consumeInput inputSideLength (\sideLength -> do
            consumeInput inputMines (\mi -> do
              let widthInput = read sideLength :: Int
                  heightInput = widthInput
                  numberOfMines = read mi :: Int

              boardRef <- liftIO $ newIORef (board widthInput heightInput numberOfMines 1 )

              let mkString s = UI.string s # set UI.class_ "string"
              let nFields = [0..(widthInput * heightInput) - 1]

              getBody view #+ [UI.div #. "boardWrapper" #+ [UI.tr #+
                  [
                  do
                      let pos = h + w * heightInput
                      button <- UI.button
                        # set UI.class_ "field"
                        # set UI.type_ "button"
                        #+ [string "_"]
                        # set UI.id_ ("field_" ++ (show pos))

                      on UI.click button $ \_ -> do
                          rBoard <- liftIO $ readIORef boardRef
                          isFlagMode <- liftIO $ readIORef refIsFlagMode
                          if isFlagMode then do
                              liftIO $ writeIORef boardRef (flagField rBoard w h)
                          else do
                              liftIO $ writeIORef boardRef (checkedRevealField rBoard w h)

                          rBoard <- liftIO $ readIORef boardRef
                          showFieldOnGUI view rBoard (width rBoard * height rBoard - 1)
                          let state = gameState rBoard
                              revealedBoard = revealBoard rBoard (width rBoard * height rBoard)
                          if state == GameLost then do
                              endingLabel <- UI.h1 # set UI.text "Game over, you lost"
                              getBody view #+ [element endingLabel]
                              showFieldOnGUI view revealedBoard (width rBoard * height rBoard - 1)
                              liftIO $ putStrLn (show revealedBoard)
                              liftIO $ putStrLn "Game over, you dun goofed"
                          else if state == GameWon then do
                              endingLabel <- UI.h1 # set UI.text "Game over, you won"
                              getBody view #+ [element endingLabel]
                              showFieldOnGUI view revealedBoard (width rBoard * height rBoard - 1)
                              liftIO $ putStrLn (show revealedBoard)
                              liftIO $ putStrLn "Game over, you won"
                          else do
                              liftIO $ putStrLn (show rBoard)
                          return ()
                      return (button)
                  | w <- [0..widthInput - 1]]
                  | h <- [0..heightInput - 1]]]
              return ()
              ))

showFieldOnGUI :: Window -> Board -> Int -> UI Element

showFieldOnGUI view brd 0 = do
    btn <- getElementById view "field_0"
    case btn of
        Nothing -> error "Something smells...fishy"
        Just el -> element el # set UI.text (show (fields brd !! 0))

showFieldOnGUI view brd index = do
    let x = mod index (width brd)
        y = quot index (height brd)

    liftIO $ putStrLn (show(index))
    btn <- getElementById view ("field_" ++ show(index))
    case btn of
        Nothing -> error ("Something smells...fishy index=" ++ show(index))
        Just el -> element el # set UI.text (show (fields brd !! index))
    showFieldOnGUI view brd (index - 1)

consumeInput :: Element -> (String -> UI ()) -> UI ()
consumeInput element consumer = do
    let theValue = get value element
    theValue >>= consumer
