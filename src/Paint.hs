module Paint where

import Game
import Graphics.Gloss

screenHeight = 600
screenWidth = 600;
cellHeight = screenHeight / 3
cellWidth = screenWidth / 3


getGridPath x = [ line [(x * cellWidth - 300, 0.0 + 300)
                       , (x * cellWidth - 300, (-screenHeight)/2)
                       ]
                , line [ (0 - 300, x * cellHeight -300)
                       , (screenWidth/2, x * cellHeight - 300)
                       ]
                ]       

boardGrid = pictures (concatMap getGridPath [0..3])

boardGameOn board = undefined
boardGameOver winner board = undefined

winnerBoard = boardGrid

worldToPicture :: World -> Picture
worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> winnerBoard    
