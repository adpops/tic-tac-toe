module Main where

import Graphics.Gloss

worldToPicture = undefined
inputEventHandler = undefined

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

window = InWindow "Tic-Tac_Toe" (500, 500) (500, 250)

main :: IO ()
main = play window white 30 startWorld worldToPicture inputEventHandler (const id)
