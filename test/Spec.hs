import Graphics.Gloss

picList = pictures ((pic):(lineLoop [(-300, 300), (-100, 300), (-100, 100), (-300, 100)]):[]) 
pic = Color white (Polygon [(-300, 300), (-100, 300), (-100, 100), (-300, 100)])

main :: IO ()
main = display (InWindow "Tic-Tac-Toe" (600, 600) (500, 250)) white picList
