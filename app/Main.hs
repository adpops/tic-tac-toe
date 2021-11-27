module Main where

import Graphics.Gloss
import Game

inputEventHandler = undefined
worldToPicture = undefined

window = InWindow "Tic-Tac_Toe" (500, 500) (500, 250)

main :: IO ()
main = play window white 30 startWorld worldToPicture inputEventHandler (const id)
