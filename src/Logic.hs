module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
--trace ("First val: " ++ (show (fst mousePos)) ++ " Second val: " ++ (show (snd mousePos)) ++ "\n")

getTileX :: Float -> Board -> Int
getTileX _ [a] = fst (fst a)
getTileX x board = if x >= (fromIntegral (-windowWidth))/2 + tileWidth * boardX && x < (fromIntegral (-windowWidth))/2 + tileWidth * (boardX + 1) then 
                     fst (fst (head board))
                 else 
                     getTileX x (tail board)
                 where boardX = fromIntegral (fst (fst (head board)))    

getTileY :: Float -> Board -> Int
getTileY _ [a] = snd (fst a)
getTileY y board = if y <= (fromIntegral windowHeight)/2 + tileHeight * boardY && y > (fromIntegral windowWidth)/2 + tileHeight * (boardY + 1) then
                     snd (fst (head board))
                 else
                     getTileY y (tail board)    
                 where boardY = fromIntegral (snd (fst (head board)))    

getTile :: Int -> Int -> Board -> Tile
getTile _ _ [a] = snd a
getTile x y board = if x == boardX && y == boardY then 
                        snd (head board)
                    else 
                        getTile x y (tail board)
                    where
                        boardX = fst (fst (head board))
                        boardY = snd (fst (head board))

getClickedTile :: Board -> (Float, Float) -> Tile
getClickedTile board pos = getTile (getTileX (fst pos) board) (getTileY (snd pos) board) board

changeBoard :: Board -> Player -> (Float, Float) -> Board
changeBoard board player pos = board

changePlayer :: Board -> Player -> Player
changePlayer board player = player

isGameOver :: Board -> Player -> GameState
isGameOver board player = GameOn

playGame :: World -> (Float, Float) -> World
playGame world mousePos = if getClickedTile (board world) mousePos == Nothing then
                              World 
                              { board = changeBoard (board world) (player world) mousePos
                              , player = changePlayer (board world) (player world)
                              , state = isGameOver (board world) (player world)
                              }
                          else 
                              world

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) world = 
    case state world of
        GameOn -> playGame world mousePos
        GameOver _ -> startWorld
inputEventHandler _ world = world        