module Game where

import Graphics.Gloss
import Data.Ix

data Player = Player1 | Player2 deriving (Show, Eq) -- Player1 = X's, Player2 = O's
type Tile = Maybe Player -- Individual tiles of board
data GameState = GameOn | GameOver (Maybe Player) deriving (Show, Eq)
type Board = [((Int, Int), Tile)]


data World = World 
                 { board  :: Board
                 , player :: Player
                 , state  :: GameState
                 } deriving (Show, Eq)

startWorld = World 
              { board  = zip (range ((0, 0), (numOfCells - 1, numOfCells - 1))) (repeat (Just Player1))
              , player = Player1
              , state  = GameOn
              }       

windowHeight = 600
windowWidth = 600
cellHeight :: Float
cellHeight = fromIntegral windowHeight / 3
cellWidth :: Float
cellWidth = fromIntegral windowWidth / 3
numOfCells = 3
window = InWindow "Tic-Tac_Toe" (windowWidth, windowHeight) (500, 250)   