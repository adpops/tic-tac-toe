module Paint where

import Game
import Graphics.Gloss


getGridPath x = [ line [(x * tileWidth - (fromIntegral windowWidth)/ 2, 0.0 + 300)
                       , (x * tileWidth - (fromIntegral windowWidth)/ 2, (fromIntegral (-windowHeight)) / 2)
                       ]
                , line [ (0 - (fromIntegral windowHeight) / 2, x * tileHeight - (fromIntegral windowHeight) / 2)
                       , ((fromIntegral windowWidth) / 2, x * tileHeight - (fromIntegral windowHeight) / 2)
                       ]
                ] 
boardGrid :: Picture
boardGrid = pictures (concatMap getGridPath [0.. (fromIntegral numOfCells)])
                                    
xTile :: Color -> Picture
xTile col = pictures [ Color col (rotate 45.0 $ rectangleSolid ((min tileWidth tileHeight) * 0.75) 10.0)
                 , Color col (rotate (-45.0) $ rectangleSolid ((min tileWidth tileHeight) * 0.75) 10.0) ]

oTile :: Color -> Picture
oTile col = Color col (thickCircle ((min tileWidth tileHeight) * 0.25) 10.0)

translatePic :: Picture -> (Int, Int) -> Picture
translatePic tile (x, y) = translate newX newY tile where
    newX = (-tileWidth) + (fromIntegral x) * tileWidth
    newY = tileHeight - (fromIntegral y) * tileHeight
    
getXPicture :: Color -> [(Int, Int)] -> Picture    
getXPicture col lst = pictures $ map (translatePic (xTile col)) lst
getOPicture :: Color -> [(Int, Int)] -> Picture  
getOPicture col lst = pictures $ map (translatePic (oTile col)) lst

findTileXY :: Board -> Tile -> [(Int, Int)]
findTileXY [] _ = []
findTileXY board currPlayer = if (snd $ head board) == currPlayer then 
                                     (fst (head board)):(findTileXY (tail board) currPlayer)
                                  else
                                      findTileXY (tail board) currPlayer

boardGameOn :: [((Int, Int), Tile)] -> Picture
boardGameOn board = pictures (xPic:oPic:boardGrid:[]) where
    xPic = getXPicture red (findTileXY board (Just Player1))
    oPic = getOPicture blue (findTileXY board (Just Player2))

boardGameOver :: (Maybe Player) -> Board -> Picture
boardGameOver winner board = do 
    let col | winner == (Just Player1) =  red 
            | winner == (Just Player2) = blue
            | otherwise = (greyN 0.7)
    pictures [ boardGrid 
             , getXPicture col (findTileXY board (Just Player1))
             , getOPicture col (findTileXY board (Just Player2))]

worldToPicture :: World -> Picture
worldToPicture world = 
    case state world of
        GameOn -> boardGameOn (board world)
        GameOver winner -> boardGameOver winner (board world)
