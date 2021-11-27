module Paint where

import Game

boardGameOn board = undefined

boardGameOver winner board = undefined


worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> boardGameOver winner (board world)
