module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) world = undefined