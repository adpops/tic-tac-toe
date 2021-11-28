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

xTile x y = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

oTile x y = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

getXPicture lst = undefined
getYPicture lst = undefined

findTileValues [] _ = []
findTileValues board currPlayer = if (snd $ head board) == currPlayer then 
                                     (fst (head board)):(findTileValues (tail board) currPlayer)
                                  else
                                      findTileValues (tail board) currPlayer

boardGameOn :: [((Int, Int), Tile)] -> Picture
boardGameOn board = pictures (xPic ++ oPic) where
    xPic = getXPicture (findTileValues board (Just Player1))
    oPic = getYPicture (findTileValues board (Just Player2))

boardGameOver winner board = boardGrid


worldToPicture :: World -> Picture
worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> boardGameOver winner board    
