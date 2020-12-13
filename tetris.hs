module Tetris where

import System.Random

--                       Current  Next
type Game = (Board,Score, Piece, Piece)

type Board = [[Block]]

type Score = Int

type Coord = (Int,Int)

type Piece = ([Coord], Dir, Format)

data Dir = North | South | West | East deriving (Show,Eq) 

data Format = I | L | S | W | O | Z | C  deriving (Show,Eq) 

data Block = Bg | Normal | Border deriving (Show,Eq) 

pieces :: [Format]
pieces = [I,L,S,W,O,Z,C]

fromJust :: Maybe a -> a
fromJust (Just t) = t

--Score
--n Lines destroyed -> 2^n pts
--

-- Test board-------------------------------------------------------------------------------------
testBoard :: Board
testBoard = replicate 10 Border : (replicate 8 ( [Border] ++ replicate 8 Normal ++ [Border])) ++ [replicate 10 Border]

--Initial Board
initialBoard :: Board
initialBoard = createBoard 10 20 


--Creates Board-----------------------------------------------------------------------------------
createBoard :: Int -> Int -> Board
createBoard li col = createAux li col li
                        where createAux :: Int -> Int -> Int -> Board
                              createAux 0 _ _ = []
                              createAux li col ini | li==ini || li==1 = replicate col Border : createAux (li-1) col ini
                                                   | otherwise =  ([Border] ++ replicate (col-2) Bg ++ [Border]): createAux (li-1) col ini


-- Checks if we got a full line in the board and updates the score and the board-------------------
updateGame :: Game -> Game
updateGame (board, score,p,np) = (removeFullLines board, newScore, p, np)
                          where newScore = case getScore board of 
                                                1 -> score
                                                n -> 2^n + score


-- Get the score of a game state.
getScore :: Board -> Score
getScore = (2^) . foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 

-- Remove the full lines of a board and add new empty ones------------------------------------------
removeFullLines :: Board -> Board
removeFullLines b =  head b : replicate fullLines ([Border] ++ replicate ( flip (-) 2 $ length $ head b) Bg ++ [Border]) ++ tail (filter (not . checkFullLine) b)
                  where fullLines = foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 b
                              

--Replace Block in Board-----------------------------------------------------------------------------
replace :: Board -> Int -> Int -> Block -> Board
replace [] _ _ _ = []
replace l@(h:t) x y b | x<0 || y<0 || length h <=x ||  (length l<=y) = l
                          | y>0 = h : replace t x (y-1) b
                          | y==0 = replaceAux h x b : t


replaceAux :: [Block] -> Int -> Block -> [Block]
replaceAux (h:t) 0 p = p:t
replaceAux (h:t) x p = h : replaceAux t (x-1) p


--Check if Line is full (Border [Normal...,Normal] Border)--------------------------------------------
checkFullLine :: [Block] -> Bool
checkFullLine l = and $ (map (\x -> x==Normal) ((init . tail) l))

--Get Block--------------------------------------------------------------------------------------------
getBlock :: Board -> (Int,Int) -> Maybe Block
getBlock [] _ = Nothing
getBlock l@(h:t) (x,y)  | x<0 || y<0 || length h <=x ||  (length l<=y) = Nothing
                        | y>0 = getBlock t (x,(y-1))
                        | y==0 = getBlockAux h x


getBlockAux :: [Block] -> Int -> Maybe Block
getBlockAux (h:t) 0 = Just h 
getBlockAux (h:t) x = getBlockAux t (x-1)

--Movie Block to the right NEEDS TO BE TESTED-------------------------------------------------------------------------------
moveRight :: Board -> (Int,Int) -> Board
moveRight b@(h:t) (x,y) | getBlock b (x,y) == Nothing = b
                        | fromJust (getBlock b (x,y)) /= Normal = b
                        | fromJust (getBlock b (x+1,y)) /= Bg = b
                        | otherwise = replace (replace b (x+1) y (fromJust (getBlock b (x,y)))) x y Bg 

--Movie Block to the left NEEDS TO BE TESTED-------------------------------------------------------------------------------
moveLeft :: Board -> (Int,Int) -> Board
moveLeft b@(h:t) (x,y) | getBlock b (x,y) == Nothing = b
                       | fromJust (getBlock b (x,y)) /= Normal = b
                       | fromJust (getBlock b (x-1,y)) /= Bg = b
                       | otherwise = replace (replace b (x-1) y (fromJust (getBlock b (x,y)))) x y Bg 

--Move Block downwards----------------------------------------------------------------------------------
moveDown :: Board -> (Int,Int) -> Board
moveDown b@(h:t) (x,y) | getBlock b (x,y) == Nothing = b
                       | fromJust (getBlock b (x,y)) /= Normal = b
                       | fromJust (getBlock b (x,y+1)) /= Bg = b
                       | otherwise = replace (replace b x (y+1) (fromJust (getBlock b (x,y)))) x y Bg 


--Get Next Piece------------------------------------------------------------------------------------------------------
getNextPiece :: IO Piece
getNextPiece = do 
            random <- randomRIO (0,6)
            let randomPiece = initFormat $ pieces !! random
            return randomPiece

--Initial format coords------------------------------------------------------------------------------------------------------
initFormat :: Format -> Piece
initFormat I = ([(3,1),(4,1),(5,1),(6,1)], West, I)
initFormat O = ([(4,1),(5,1),(4,2),(5,2)], South, O)
initFormat S = ([(4,1),(5,1),(3,2),(4,2)], East, S)
initFormat W = ([(4,1),(3,2),(4,2),(5,2)], North, W)
initFormat L = ([(5,1),(3,2),(4,2),(5,2)], West, L)
initFormat Z = ([(3,1),(4,1),(4,2),(5,2)], West, Z)
initFormat C = ([(3,1),(3,2),(4,2),(5,2)], East, C)

--Rotate Piece NEEDS TESTING------------------------------------------------------------------------------------------------------
rotatePiece :: Piece -> Piece 
rotatePiece (c,d,O) = (c,d,O)

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, I) | dir == North =  ([(x1-1,y1+1),(x2,y2),(x3+1,y3-1),(x4+2,y4-2)], East,I) 
                                                        | dir == East = ([(x1+2,y1-1),(x2+1,y2),(x3,y3+1),(x4-1,y4+2)], South,I) 
                                                        | dir == South = ([(x1-2,y1+1),(x2-1,y2),(x3,y3-1),(x4+1,y4-2)], West,I)
                                                        | dir == West = ([(x1+1,y1-1),(x2,y2),(x3-1,y3+1),(x4-2,y4+2)], North,I) 

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, S) | dir == East = ([(x1,y1-1),(x2-1,y2),(x3+2,y3-1),(x4+1,y4)], South,S)   
                                                        | dir == South =  ([(x1,y1+1),(x2+1,y2),(x3-2,y3+1),(x4-1,y4)], West,S) 
                                                        | dir == West = ([(x1-1,y1-1),(x2-2,y2),(x1,y1),(x4,y4)], North,S)
                                                        | dir == North = ([(x3,y3),(x2+2,y2),(x3-1,y3+1),(x4,y4)], East,S)

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, W) | dir == North =  ([(x1,y1),(x3,y3),(x4,y4),(x3,y3+1)], East,W)
                                                        | dir == East = ([(x1-1,y1+1),(x2,y2),(x3,y3),(x4,y4)], South,W)
                                                        | dir == South = ([(x2,y2-1),(x1,y1),(x2,y2),(x4,y4)], West,W)
                                                        | dir == West = ([(x1,y1),(x2,y2),(x3,y3),(x4+1,y4-1)], North,W) 

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, L) | dir == West =  ([(x1-1,y1-1),(x2+1,y2-1),(x3,y3),(x4,y4)], North,L)
                                                        | dir == North = ([(x1-1,y1+1),(x2,y2),(x3-1,y3+1),(x4-2,y4)], East,L)
                                                        | dir == East = ([(x1+1,y1-1),(x2+1,y2-1),(x3,y3),(x4+2,y4)], South,L)
                                                        | dir == South = ([(x3,y3),(x3-2,y3),(x3-1,y3),(x4,y4)], West, L)

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, Z) | dir == West =  ([(x1+2,y1-1),(x2,y2),(x3+1,y3-1),(x3,y3)], North,Z)
                                                        | dir == North = ([(x1-2,y1+1),(x2,y2),(x4,y4),(x4+1,y4)], East,Z)
                                                        | dir == East = ([(x1+1,y1-1),(x1,y1),(x2,y2),(x4-2,y4)], South,Z)
                                                        | dir == South = ([(x2,y2),(x3,y3),(x4+1,y4),(x4+2,y4)], West,Z)
 
rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, C) | dir == East =  ([(x1,y1-1),(x1+1,y1-1),(x1,y1),(x2,y2)], South,C)
                                                        | dir == South = ([(x1-1,y1+1),(x3,y3),(x2,y2+1),(x4+1,y4)], West,C)
                                                        | dir == West = ([(x3,y3-1),(x3,y3),(x4,y4),(x4-1,y4)], North,C)
                                                        | dir == North = ([(x1-1,y1+1),(x4,y4),(x3,y3),(x3+1,y3)], East,C)

--If piece can rotate NEEDS TESTING------------------------------------------------------------------------------------------------------
--gets Coords from Piece
firstP :: Piece -> [Coord]
firstP (coords,dir,format) = coords

canRotate :: Game -> Bool
canRotate (b,s,p,np) = all (\x -> x == Just Bg)  $ map (\x -> getBlock b x)  $ firstP (rotatePiece p)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
posBoard :: Board -> Int -> [[(Int,Int)]] -> [[(Int,Int)]]
posBoard _ 10 res = res
posBoard (h:t) li [[]] = posBoard t (li+1) ([posInLine h (li,0)]) 
posBoard (h:t) li res = posBoard t (li+1) (res ++ [posInLine h (li,0)])

posInLine :: [Block] -> (Int,Int) -> [(Int,Int)]
posInLine [] (li,col)    = []
posInLine (h:t) (li,col) = (li,col) : posInLine t (li,col+1)