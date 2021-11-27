module Game where

import Main
import Graphics.Gloss

boardGameOn board = undefined

boardGameOver winner board = if winner == Nothing then 
                                display (InWindow "Game Over" (200, 200) (500, 250)) white (Text "The Game is Tied")
                             else 
                                display (InWindow "Game Over" (200, 200) (500, 250)) white (Text ("The winner is " ++ (show winner)))


worldToPicture gameInfo = 
    case state gameInfo of
        GameOn -> boardGameOn (board gameInfo)
        GameOver winner -> boardGameOver winner (board gameInfo)