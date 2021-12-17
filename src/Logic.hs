module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

getTileX :: Float -> Board -> Int
--getTileX _ [] = error "x coordinates of mouse click not found"
getTileX _ [a] = fst (fst a)
getTileX x board = if x >= (fromIntegral (-windowWidth))/2 + tileWidth * boardTileX && x < (fromIntegral (-windowWidth))/2 + tileWidth * (boardTileX + 1) then 
                     fst (fst (head board))
                 else 
                     getTileX x (tail board)
                 where boardTileX = fromIntegral (fst (fst (head board)))    

getTileY :: Float -> Board -> Int
--getTileY _ [] = error "y coordinates of mouse click not found"
getTileY _ [a] = snd (fst a)
getTileY y board = if y <= (fromIntegral windowHeight)/2 - tileHeight * boardTileY && y > (fromIntegral windowWidth)/2 - tileHeight * (boardTileY + 1) then
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

checkWinner :: Board -> Player -> [[(Int, Int)]] -> Bool
checkWinner _ _ [] = True
checkWinner board player lst = if snd (head (getTile x y board)) == (Just player) then 
                                   checkWinner board player newLst
                               else 
                                   False
                               where x = fst (head (head lst))
                                     y = snd (head (head lst))
                                     newLst = (tail (head lst)):(tail lst)

didPlayerWin :: Board -> Player -> Bool
didPlayerWin board player = checkWinner board player row || checkWinner board player col || checkWinner board player diag
                            where 
                                  row  = [[(x, y) | x <- [0..numOfCells-1]] | y <- [0..numOfCells-1]]
                                  col  = [[(x, y) | y <- [0..numOfCells-1]] | x <- [0..numOfCells-1]]
                                  diag = [[(x, x) | x <- [0..numOfCells-1]], [(x, y) | x <- [0..numOfCells-1], let y = (numOfCells-1) - x]]

isBoardFull :: Board -> Bool
isBoardFull [] = True
isBoardFull board = if (snd (head board)) == Nothing then 
                        False
                    else
                        isBoardFull (tail board)    

isGameOver :: Board -> Player -> GameState
isGameOver board player | didPlayerWin board player = GameOver (Just player)
                        | isBoardFull board         = GameOver Nothing
                        | otherwise                 = GameOn

playGame :: World -> (Float, Float) -> World
playGame world mousePos = if (snd (head currTileInfo)) == Nothing then
                              World 
                              { board  = newBoard
                              , player = newPlayer
                              , state  = newState
                              }
                          else
                              world
                          where currTileInfo = getClickedTile (board world) mousePos
                                newBoard     = changeBoard (board world) (player world) currTileInfo
                                newPlayer    = changePlayer newBoard (player world)
                                newState     = isGameOver newBoard newPlayer

inputEventHandler :: Event -> World -> World
inputEventHandler (EventKey (MouseButton LeftButton) Down _ mousePos) world = 
    case state world of
        GameOn -> playGame world mousePos
        GameOver _ -> startWorld
inputEventHandler _ world = world        