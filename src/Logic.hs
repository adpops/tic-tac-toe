module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
--trace ("First val: " ++ (show (fst mousePos)) ++ " Second val: " ++ (show (snd mousePos)) ++ "\n")

getTileX :: Float -> Board -> Int
getTileX _ [a] = fst (fst a)
getTileX x board = if x >= (fromIntegral (-windowWidth))/2 + tileWidth * boardTileX && x < (fromIntegral (-windowWidth))/2 + tileWidth * (boardTileX + 1) then 
                     fst (fst (head board))
                 else 
                     getTileX x (tail board)
                 where boardTileX = fromIntegral (fst (fst (head board)))    

getTileY :: Float -> Board -> Int
getTileY _ [a] = snd (fst a)
getTileY y board = if y <= (fromIntegral windowHeight)/2 + tileHeight * boardTileY && y > (fromIntegral windowWidth)/2 + tileHeight * (boardTileY + 1) then
                     snd (fst (head board))
                 else
                     getTileY y (tail board)    
                 where boardTileY = fromIntegral (snd (fst (head board)))    

getTile :: Int -> Int -> Board -> Tile
getTile _ _ [a] = snd a
getTile x y board = if x == boardTileX && y == boardTileY then 
                        snd (head board)
                    else 
                        getTile x y (tail board)
                    where
                        boardTileX = fst (fst (head board))
                        boardTileY = snd (fst (head board))

getClickedTile :: Board -> (Float, Float) -> Tile
getClickedTile board pos = getTile (getTileX (fst pos) board) (getTileY (snd pos) board) board

changeBoard :: Board -> Player -> (Float, Float) -> Board
changeBoard [] _ _ = []
changeBoard board player pos = if (boardTileX, boardTileY) == fst (head board) then 
                                   (fst (head board), (Just player)):(changeBoard (tail board) player pos)
                               else 
                                   (head board):(changeBoard (tail board) player pos)
                               where
                                   boardTileX = getTileX (fst pos) board
                                   boardTileY = getTileY (snd pos) board   

changePlayer :: Board -> Player -> Player
changePlayer board player = if player == Player1 then Player2 else Player1

isGameOver :: Board -> Player -> GameState
isGameOver board player = GameOn

playGame :: World -> (Float, Float) -> World
playGame world mousePos = if tile == Nothing then
                              trace "Nothing Tile"  
                              World 
                              { board = changeBoard (board world) (player world) mousePos
                              , player = changePlayer (board world) (player world)
                              , state = isGameOver (board world) (player world)
                              }
                          else
                              trace "occupied tile"   
                              world
                          where tile = getClickedTile (board world) mousePos

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) world = 
    case state world of
        GameOn -> playGame world mousePos
        GameOver _ -> startWorld
inputEventHandler _ world = world        