module Modules.Game where

import Graphics.Gloss
import Modules.Tetris as Tetris

window :: Display
window = InWindow "Tetris" (700, 700) (500, 600)

background :: Color
background = black

drawing :: Picture
drawing = circle 80

main :: IO ()
main = play
        window
        black
        20
        (1,2)
        (\ _ -> drawing)
        (\ _ w -> w)
        (\ f w -> w)