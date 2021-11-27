module Game where

import Graphics.Gloss

data Player = Player1 | Player2 deriving (Show, Eq) -- Player1 = X's, Player2 = O's
type Cell = Maybe Player -- Individual tiles of board
data GameState = GameOn | GameOver (Maybe Player) deriving (Show, Eq)
type Board = [Cell]
data GameInfo = GameInfo 
                 { board  :: Board
                 , player :: Player
                 , state  :: GameState
                 } deriving (Show, Eq)

startWorld = GameInfo 
              { board  = [ Nothing, Nothing, Nothing,
                           Nothing, Nothing, Nothing,
                           Nothing, Nothing, Nothing ]
              , player = Player1
              , state  = GameOn
              }

--gameOverBoard board = pictures[Nothing]

boardGameOn board = undefined

boardGameOver winner board = do 
    if winner == Nothing then 
        display (InWindow "Game Over" (200, 200) (500, 250)) white (Text "The Game is Tied")
    else 
        display (InWindow "Game Over" (200, 200) (500, 250)) white (Text ("The winner is " ++ (show winner)))
    --color green Blank


{-worldToPicture gameInfo = 
    case state gameInfo of
        GameOn -> boardGameOn (board gameInfo)
        GameOver winner -> boardGameOver winner (board gameInfo)
        -}