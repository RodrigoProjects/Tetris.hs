module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tetris as Tetris 

type Imgs = (Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture)
type State = (Int, Game, Imgs)

window :: Display
window = InWindow "Tetris" (1200, 1000) (0,0)

background :: Color
background = black

-- Draw the Game --------------------------------------------------
drawGame :: State -> IO Picture
drawGame (0, (b, s, c, n), imgs) = return $ Translate (-400) (400) $ Pictures $ drawBoard b (0,0) imgs ++ drawCurrentPiece c imgs ++ drawScore s ++ drawNextPiece n imgs
drawGame (1, (b, s, c, n), imgs) = return $ Pictures [Translate (-250) (200) $ Color white $ Scale 0.6 0.6 $ Text "Game Over", Translate (-80) (50) $ Color green $ Scale 0.5 0.5 $ Text $ show s] 
drawGame _ = undefined

drawNextPiece :: Piece -> Imgs -> [Picture]
drawNextPiece ([], _, _, _) _ = [Color white $ Translate (510) (-650) $ Text "Next"]
drawNextPiece (((x,y):t), dir, f, c) imgs 
        = Translate (fromIntegral $ x * 40 + 460) (fromIntegral $ y * (-40) - 700) (colorToPicture c imgs) : drawNextPiece (t, dir, f, c) imgs

drawScore :: Int -> [Picture]
drawScore i = [Color white $ Translate (510) (-150) $ Text "Score", Color green $ Translate (510) (-300) $ Scale 0.8 0.8 $ Text $ show i]

drawBoard :: Board -> Coord -> Imgs -> [Picture]
drawBoard [] _ _ = []
drawBoard (h:t) (x,y) imgs@(border, bg, yellow, red, pink, orange, green, darkBlue, blue) = 
        drawRow (x,y) h imgs ++ drawBoard t (x, y-40) imgs

drawRow :: Coord -> [Block] -> Imgs -> [Picture]
drawRow _ [] _ = []
drawRow (x, y) (h:t) imgs@(border, bg, yellow, red, pink, orange, green, darkBlue, blue) =
        Translate (fromIntegral x) (fromIntegral y) (blockToPicture h imgs) : drawRow (x+40, y) t imgs

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
 
drawCurrentPiece :: Piece -> Imgs -> [Picture]
drawCurrentPiece ([], _, _, _) _ = []
drawCurrentPiece (((x,y):t), dir, f, c) imgs 
        = Translate (fromIntegral $ x * 40) (fromIntegral $ y * (-40)) (colorToPicture c imgs) : drawCurrentPiece (t, dir, f, c) imgs

colorToPicture :: Colour -> Imgs -> Picture
colorToPicture c (border, bg, yellow, red, pink, orange, green, darkBlue, blue) 
        = case c of
             Blue -> blue
             DarkBlue -> darkBlue
             Green -> green
             Orange -> orange
             Pink -> pink
             Red -> red
             Yellow -> yellow 

timeToWorld :: Float -> State -> IO State 
timeToWorld _ ss@(s, g@(b, score, cur@(coords, dir, form, cor), nex), imgs) 
        | canMove g South = return (s, (b, score, movePiece cur South, nex), imgs)
        | otherwise = do
                nextPiece <- getNextPiece 
                case canReplace (b, score, nex, nex) of
                        True -> return (0, updateGame (replaceMult b coords (Normal cor), score, nex, nextPiece), imgs)
                        _    -> return (1, g, imgs) 

eventHandler :: Event -> State -> IO State
eventHandler (EventKey (Char 's') Down _ _) ss@(0, g@(b, score, cur@(coords, dir, form, cor), nex), imgs) 
        | canMove g South = return (0, (b, score + 1, movePiece cur South, nex), imgs)
        | otherwise = do
                nextPiece <- getNextPiece 
                case canReplace (b, score, nex, nex) of
                        True -> return (0, updateGame (replaceMult b coords (Normal cor), score, nex, nextPiece), imgs)
                        _    -> return (1, g, imgs) 
                
eventHandler (EventKey (Char 'a') Down _ _) ss@(0, g@(b, score, cur@(coords, dir, form, cor), nex), imgs) 
        | canMove g West = return (0, (b, score, movePiece cur West , nex), imgs)
        | otherwise = return ss
eventHandler (EventKey (Char 'd') Down _ _) ss@(0, g@(b, score, cur@(coords, dir, form, cor), nex), imgs) 
        | canMove g East  = return (0, (b, score, movePiece cur East , nex), imgs)
        | otherwise = return ss
eventHandler (EventKey (Char 'r') Down _ _) ss@(0, g@(b, score, cur@(coords, dir, form, cor), nex), imgs) 
        | canRotate g = return (0, Tetris.rotate g, imgs)
        | otherwise = return ss
eventHandler _ s = return s

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
        playIO
          window
          black
          1
          (0,game, (Scale 0.4 0.4 border, Scale 0.4 0.4 bg, Scale 0.4 0.4 yellow, Scale 0.4 0.4 red, Scale 0.4 0.4 pink, Scale 0.4 0.4 orange, Scale 0.4 0.4 green, Scale 0.4 0.4 darkBlue, Scale 0.4 0.4 blue))
          drawGame
          eventHandler
          timeToWorld