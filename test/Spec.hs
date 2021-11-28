import Graphics.Gloss


data Player = Player1 | Player2 deriving (Show, Eq) -- Player1 = X's, Player2 = O's
--type Tile = [Path] -- Individual tiles of board

tileSize = 200
boardSize = 600
{-tile1 = (Color white (Polygon [(-300, 300), (-100, 300), (-100, 100), (-300, 100)])):
    (lineLoop [(-300, 300), (-100, 300), (-100, 100), (-300, 100)]):[]
tile2 = (Color white (Polygon [(-100, 300), (100, 300), (100, 100), (-100, 100)])):
    (lineLoop [(-100, 300), (100, 300), (100, 100), (-100, 100)]):[]
-}
picList = pictures [translate 0 (-50) (Color red (line [(-275, 275), (-63, 275)])), (line [(-275, 275), (-63, 275)])]

main :: IO ()
main = display (InWindow "Tic-Tac-Toe" (boardSize, boardSize) (500, 250)) white picList
