module Paint where

import Game
import Graphics.Gloss

boardGameOn board = undefined

boardGameOver winner board = undefined

worldToPicture :: World -> Picture
worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> boardGameOver winner (board world)
