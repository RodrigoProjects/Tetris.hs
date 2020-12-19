module Game where

import Graphics.Gloss
import Tetris as Tetris 

type Imgs = (Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture)
type State = (Int, Game, Imgs)

window :: Display
window = InWindow "Tetris" (700, 700) (500, 600)

background :: Color
background = black

-- Draw the Game --------------------------------------------------
drawGame :: State -> Picture
drawGame (0, (b, s, c, n), imgs) = Pictures  $ drawBoard b (-175,0) imgs
drawGame _ = circle 80

drawBoard :: Board -> Coord -> Imgs -> [Picture]
drawBoard [] _ _ = []
drawBoard (h:t) (x,y) imgs@(border, bg, yellow, red, pink, orange, green, darkBlue, blue) = 
        drawRow (x,y) h imgs ++ drawBoard t (x, y+20) imgs

drawRow :: Coord -> [Block] -> Imgs -> [Picture]
drawRow _ [] _ = []
drawRow (x, y) (h:t) imgs@(border, bg, yellow, red, pink, orange, green, darkBlue, blue) =
        Translate (fromIntegral x) (fromIntegral y) (blockToPicture h imgs) : drawRow (x+20, y) t imgs

-- Black | Blue | DarkBlue | Green | Orange | Pink | Red | Yellow
blockToPicture :: Block -> Imgs -> Picture
blockToPicture Bg (border, bg, yellow, red, pink, orange, green, darkBlue, blue) = bg
blockToPicture Border (border, bg, yellow, red, pink, orange, green, darkBlue, blue) = border
blockToPicture (Normal c) (border, bg, yellow, red, pink, orange, green, darkBlue, blue) = 
        case c of
            Blue -> blue
            DarkBlue -> darkBlue
            Green -> green
            Orange -> orange
            Pink -> pink
            Red -> red
            Yellow -> yellow 
 
drawing :: Picture
drawing = circle 80

main :: IO ()
main = do
        game <- initGame
        border <- loadBMP "assets/border.bmp"
        bg <- loadBMP "assets/background.bmp"
        yellow <- loadBMP "assets/yellow.bmp"
        red <- loadBMP "assets/red.bmp"
        pink <- loadBMP "assets/pink.bmp"
        orange <- loadBMP "assets/orange.bmp"
        green <- loadBMP "assets/green.bmp"
        darkBlue <- loadBMP "assets/darkBlue.bmp"
        blue <- loadBMP "assets/blue.bmp"
        play
          window
          black
          20
          (0,game, (Scale 0.2 0.2 border, Scale 0.2 0.2 bg, Scale 0.2 0.2 yellow, Scale 0.2 0.2 red, Scale 0.2 0.2 pink, Scale 0.2 0.2 orange, Scale 0.2 0.2 green, Scale 0.2 0.2 darkBlue, Scale 0.2 0.2 blue))
          drawGame
          (\ _ w -> w)
          (\ f w -> w)