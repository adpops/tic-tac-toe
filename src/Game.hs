module Game where

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
              { board  = zip (range ((0, 0), (2, 2))) (repeat Nothing)
              , player = Player1
              , state  = GameOn
              }          