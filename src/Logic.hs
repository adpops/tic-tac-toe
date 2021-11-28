module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game

gameOnAction world = undefined

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) world = 
    case state world of
        GameOn -> gameOnAction world
        GameOver _ -> startWorld
inputEventHandler _ world = world        