module Main where

import Graphics.Gloss
import Game
import Paint
import Logic 

window = InWindow "Tic-Tac_Toe" (600, 600) (500, 250)

main :: IO ()
main = play window white 30 startWorld worldToPicture inputEventHandler (const id)
