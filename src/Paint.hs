module Paint where

import Game
import Graphics.Gloss

getGridPath x = [ line [(x * cellWidth - (fromIntegral windowWidth)/(fromIntegral numOfCells - 1), 0.0 + 300)
                       , (x * cellWidth - (fromIntegral windowWidth)/(fromIntegral numOfCells - 1), (fromIntegral (-windowHeight))/2)
                       ]
                , line [ (0 - (fromIntegral windowHeight) / (fromIntegral numOfCells - 1), x * cellHeight - (fromIntegral windowHeight) / (fromIntegral numOfCells - 1))
                       , (fromIntegral windowWidth/2, x * cellHeight - (fromIntegral windowHeight) / (fromIntegral numOfCells - 1))
                       ]
                ]       
boardGrid = pictures (concatMap getGridPath [0..3])

thickLine x y = Color red (Polygon [ (-275 + cellWidth * x, 275 - cellHeight * y)
                                   , (-63 + cellWidth * x, 275 - cellWidth * y)
                                   , (-63 + cellWidth * x, 265 - cellWidth * y)
                                   , (-275 + cellWidth * x, 265 - cellWidth * y)])
                                    

xTile tuple = pictures [ rotate (45.0) $ thickLine (fromIntegral (fst tuple)) (fromIntegral (snd tuple))
                       {-, rotate (-45.0) $ thickLine (fromIntegral (fst tuple)) (fromIntegral (snd tuple)) -}
                       ]

oTile x y = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

getXPicture lst = pictures [xTile (0, 0)]
getYPicture lst = Blank

findTileValues [] _ = []
findTileValues board currPlayer = if (snd $ head board) == currPlayer then 
                                     (fst (head board)):(findTileValues (tail board) currPlayer)
                                  else
                                      findTileValues (tail board) currPlayer

boardGameOn :: [((Int, Int), Tile)] -> Picture
boardGameOn board = pictures (xPic:oPic:boardGrid:[]) where
    xPic = getXPicture (findTileValues board (Just Player1))
    oPic = getYPicture (findTileValues board (Just Player2))

boardGameOver winner board = boardGrid


worldToPicture :: World -> Picture
worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> boardGameOver winner board    
