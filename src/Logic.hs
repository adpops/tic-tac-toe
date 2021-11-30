module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
--trace ("First val: " ++ (show (fst mousePos)) ++ " Second val: " ++ (show (snd mousePos)) ++ "\n")

getTileX :: Float -> Board -> Int
--getTileX _ [] = error "x coordinates of mouse click not found"
getTileX _ [a] = fst (fst a)
getTileX x board = if x >= (fromIntegral (-windowWidth))/2 + tileWidth * boardTileX && x < (fromIntegral (-windowWidth))/2 + tileWidth * (boardTileX + 1) then 
                     trace ("x coordinates of mouse : " ++ (show $ fst (fst (head board))))
                     fst (fst (head board))
                 else 
                     getTileX x (tail board)
                 where boardTileX = fromIntegral (fst (fst (head board)))    

getTileY :: Float -> Board -> Int
--getTileY _ [] = error "y coordinates of mouse click not found"
getTileY _ [a] = snd (fst a)
getTileY y board = if y <= (fromIntegral windowHeight)/2 - tileHeight * boardTileY && y > (fromIntegral windowWidth)/2 - tileHeight * (boardTileY + 1) then
                     trace ("y coordinates of mouse : " ++ (show $ snd (fst (head board))))
                     snd (fst (head board))
                 else
                     getTileY y (tail board)    
                 where boardTileY = fromIntegral (snd (fst (head board)))    

getTile :: Int -> Int -> Board -> Board
getTile _ _ [a] = [a]
getTile x y board = if x == boardTileX && y == boardTileY then 
                        [head board]
                    else 
                        getTile x y (tail board)
                    where
                        boardTileX = fst (fst (head board))
                        boardTileY = snd (fst (head board))

getClickedTile :: Board -> (Float, Float) -> Board
getClickedTile board pos = getTile (getTileX (fst pos) board) (getTileY (snd pos) board) board

changeBoard :: Board -> Player -> Board -> Board
changeBoard [] _ _ = []
changeBoard board player tileInfo = if (boardTileX, boardTileY) == fst (head board) then 
                                   (fst (head board), (Just player)):(changeBoard (tail board) player tileInfo)
                               else 
                                   (head board):(changeBoard (tail board) player tileInfo)
                               where
                                   boardTileX = fst (fst (head tileInfo))
                                   boardTileY = snd (fst (head tileInfo))  

changePlayer :: Board -> Player -> Player
changePlayer board player = if player == Player1 then Player2 else Player1

didPlayerWin :: Board -> Player -> Bool
didPlayerWin board player = False


isBoardFull :: Board -> Bool
isBoardFull [] = True
isBoardFull board = if (snd (head board)) == Nothing then 
                        False
                    else
                        isBoardFull (tail board)    

isGameOver :: Board -> Player -> GameState
isGameOver board player | didPlayerWin board player == True = GameOver (Just player)
                        | isBoardFull board  == True = GameOver Nothing
                        | otherwise = GameOn

playGame :: World -> (Float, Float) -> World
playGame world mousePos = if (snd (head currTileInfo)) == Nothing then
                              trace "Nothing Tile" 
                              World 
                              { board = newBoard
                              , player = newPlayer
                              , state = newState
                              }
                          else
                              trace "occupied tile"   
                              world
                          where currTileInfo = getClickedTile (board world) mousePos
                                newBoard = changeBoard (board world) (player world) currTileInfo
                                newPlayer = changePlayer newBoard (player world)
                                newState = isGameOver newBoard newPlayer

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Down _ mousePos) world = 
    case state world of
        GameOn -> playGame world mousePos
        GameOver _ -> startWorld
inputEventHandler _ world = world        