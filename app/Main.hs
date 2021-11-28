module Main where

import Graphics.Gloss
import Game
import Paint
import Logic 


main :: IO ()
main = play window white 30 startWorld worldToPicture inputEventHandler (const id)
--main = display window white (worldToPicture startWorld)
