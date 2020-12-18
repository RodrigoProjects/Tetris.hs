module Modules.Tetris where

import System.Random

--                       Current  Next
type Game = (Board,Score, Piece, Piece)

type Board = [[Block]]

type Score = Int

type Coord = (Int,Int)

type Piece = ([Coord], Dir, Format, Colour)

data Dir = North | South | West | East deriving (Show,Eq) 

data Format = I | L | S | W | O | Z | C  deriving (Show,Eq)

data Colour = Black | Blue | DarkBlue | Green | Orange | Pink | Red | Yellow deriving (Show,Eq)

data Block = Bg | Normal Colour | Border deriving (Show,Eq) 

pieces :: [Format]
pieces = [I,L,S,W,O,Z,C]

fromJust :: Maybe a -> a
fromJust (Just t) = t

--Score
--n Lines destroyed -> 2^n pts

-- Init Game-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- type Game = (Board,Score, Piece, Piece)
initGame :: IO Game
initGame = do
        fstPiece <- getNextPiece
        sndPiece <- getNextPiece
        return (initialBoard, 0, fstPiece, sndPiece)
      

-- Test board----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testBoard :: Board
testBoard = replicate 10 Border : (replicate 8 ( [Border] ++ replicate 8 (Normal Black) ++ [Border])) ++ [replicate 10 Border]

--Initial Board
initialBoard :: Board
initialBoard = createBoard 10 20 


--Creates Board--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
createBoard :: Int -> Int -> Board
createBoard li col = createAux li col li
                        where createAux :: Int -> Int -> Int -> Board
                              createAux 0 _ _ = []
                              createAux li col ini | li==ini || li==1 = replicate col Border : createAux (li-1) col ini
                                                   | otherwise =  ([Border] ++ replicate (col-2) Bg ++ [Border]): createAux (li-1) col ini


-- Checks if we got a full line in the board and updates the score and the board---------------------------------------------------------------------------------------------------------------
updateGame :: Game -> Game
updateGame (board, score,p,np) = (removeFullLines board, newScore, p, np)
                          where newScore = case getScore board of 
                                                1 -> score
                                                n -> n + score


-- Get the score of a game state---------------------------------------------------------------------------------------------------------------------------------------------------------------
getScore :: Board -> Score
getScore = (2^) . foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 

-- Remove the full lines of a board and add new empty ones-------------------------------------------------------------------------------------------------------------------------------------
removeFullLines :: Board -> Board
removeFullLines b =  head b : replicate fullLines ([Border] ++ replicate ( flip (-) 2 $ length $ head b) Bg ++ [Border]) ++ tail (filter (not . checkFullLine) b)
                  where fullLines = foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 b
                              

--Replace Multiple Blocks without logic--------------------------------------------------------------------------------------------------------------------------------------------------------
replaceMult :: Board -> [(Int, Int)] -> [Block] -> Board
replaceMult b (h:t) (bl:bls) = replaceMult (replace b h bl) t bls


--Replace Block in Board-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
replace :: Board -> (Int,Int) -> Block -> Board
replace [] _ _ = []
replace l@(h:t) (x,y) b | x<0 || y<0 || length h <=x ||  (length l<=y) = l
                        | y>0 = h : replace t (x,(y-1)) b
                        | y==0 = replaceAux h x b : t


replaceAux :: [Block] -> Int -> Block -> [Block]
replaceAux (h:t) 0 p = p:t
replaceAux (h:t) x p = h : replaceAux t (x-1) p


--Check if Line is full (Border [Normal...,Normal] Border)-------------------------------------------------------------------------------------------------------------------------------------
checkFullLine :: [Block] -> Bool
checkFullLine l = and $ (map isNormal ((init . tail) l))

isNormal :: Block -> Bool
isNormal (Normal _) = True
isNormal _ = False

--Get Block------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getBlock :: Board -> (Int,Int) -> Maybe Block
getBlock [] _ = Nothing
getBlock l@(h:t) (x,y)  | x<0 || y<0 || length h <=x ||  (length l<=y) = Nothing
                        | y>0 = getBlock t (x,(y-1))
                        | y==0 = getBlockAux h x


getBlockAux :: [Block] -> Int -> Maybe Block
getBlockAux (h:t) 0 = Just h 
getBlockAux (h:t) x = getBlockAux t (x-1)

--Returns Moved Piece--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
movePiece :: Piece -> Dir -> Piece
movePiece p@(coords,d,f,c) dir | dir == South = (map (\(x,y) -> (x,y + 1)) coords, d,f,c)
                               | dir == West  = (map (\(x,y) -> (x - 1,y)) coords, d,f,c)
                               | dir == East  = (map (\(x,y) -> (x + 1,y)) coords, d,f,c)

--Returns If Piece can move   NEEDS TESTING
canMove :: Game -> Dir -> Bool
canMove (b,s,p@(coords,d,f,c),np) dir = (all (\x -> x == Just Bg)  $ map (\x -> getBlock b x)  $ firstP (movePiece p dir))
            && (all isMaybeNormal  $ map (\x -> getBlock b x)  coords )

isMaybeNormal :: Maybe Block -> Bool
isMaybeNormal (Just (Normal _)) = True
isMaybeNormal _ = False

--Moves a Piece--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--move :: Game -> Dir -> Game
move g@(b,s,p,np) dir | canMove g dir = (b,s, movePiece p dir,np)
                      | otherwise = g


--Get Next Piece-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getNextPiece :: IO Piece
getNextPiece = do 
            random <- randomRIO (0,6)
            let randomPiece = initFormat $ pieces !! random
            return randomPiece

--Initial format coords------------------------------------------------------------------------------------------------------------------------------------------------------------------------
initFormat :: Format -> Piece
initFormat I = ([(3,1),(4,1),(5,1),(6,1)], West, I, Orange)
initFormat O = ([(4,1),(5,1),(4,2),(5,2)], South, O, Blue)
initFormat S = ([(4,1),(5,1),(3,2),(4,2)], East, S, Green)
initFormat W = ([(4,1),(3,2),(4,2),(5,2)], North, W, Pink)
initFormat L = ([(5,1),(3,2),(4,2),(5,2)], West, L, Yellow)
initFormat Z = ([(3,1),(4,1),(4,2),(5,2)], West, Z, Red)
initFormat C = ([(3,1),(3,2),(4,2),(5,2)], East, C, DarkBlue)

--Rotate Piece NEEDS TESTING-------------------------------------------------------------------------------------------------------------------------------------------------------------------
rotatePiece :: Piece -> Piece 
rotatePiece p@(coords, dir, O,c) = p

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, I, Orange) | dir == North =  ([(x1-1,y1+1),(x2,y2),(x3+1,y3-1),(x4+2,y4-2)], East,I, Orange) 
                                                        | dir == East = ([(x1+2,y1-1),(x2+1,y2),(x3,y3+1),(x4-1,y4+2)], South,I, Orange) 
                                                        | dir == South = ([(x1-2,y1+1),(x2-1,y2),(x3,y3-1),(x4+1,y4-2)], West,I, Orange)
                                                        | dir == West = ([(x1+1,y1-1),(x2,y2),(x3-1,y3+1),(x4-2,y4+2)], North,I, Orange) 

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, S, Green) | dir == East = ([(x1,y1-1),(x2-1,y2),(x3+2,y3-1),(x4+1,y4)], South,S, Green)   
                                                        | dir == South =  ([(x1,y1+1),(x2+1,y2),(x3-2,y3+1),(x4-1,y4)], West,S, Green) 
                                                        | dir == West = ([(x1-1,y1-1),(x2-2,y2),(x1,y1),(x4,y4)], North,S, Green)
                                                        | dir == North = ([(x3,y3),(x2+2,y2),(x3-1,y3+1),(x4,y4)], East,S, Green)

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, W,Pink) | dir == North =  ([(x1,y1),(x3,y3),(x4,y4),(x3,y3+1)], East,W, Pink)
                                                        | dir == East = ([(x1-1,y1+1),(x2,y2),(x3,y3),(x4,y4)], South,W, Pink)
                                                        | dir == South = ([(x2,y2-1),(x1,y1),(x2,y2),(x4,y4)], West,W, Pink)
                                                        | dir == West = ([(x1,y1),(x2,y2),(x3,y3),(x4+1,y4-1)], North,W, Pink) 

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, L,Yellow) | dir == West =  ([(x1-1,y1-1),(x2+1,y2-1),(x3,y3),(x4,y4)], North,L, Yellow)
                                                        | dir == North = ([(x1-1,y1+1),(x2,y2),(x3-1,y3+1),(x4-2,y4)], East,L, Yellow)
                                                        | dir == East = ([(x1+1,y1-1),(x2+1,y2-1),(x3,y3),(x4+2,y4)], South,L, Yellow)
                                                        | dir == South = ([(x3,y3),(x3-2,y3),(x3-1,y3),(x4,y4)], West, L, Yellow)

rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, Z, Red) | dir == West =  ([(x1+2,y1-1),(x2,y2),(x3+1,y3-1),(x3,y3)], North,Z, Red)
                                                        | dir == North = ([(x1-2,y1+1),(x2,y2),(x4,y4),(x4+1,y4)], East,Z, Red)
                                                        | dir == East = ([(x1+1,y1-1),(x1,y1),(x2,y2),(x4-2,y4)], South,Z, Red)
                                                        | dir == South = ([(x2,y2),(x3,y3),(x4+1,y4),(x4+2,y4)], West,Z, Red)
 
rotatePiece ([(x1,y1),(x2,y2),(x3,y3),(x4,y4)], dir, C,DarkBlue) | dir == East =  ([(x1,y1-1),(x1+1,y1-1),(x1,y1),(x2,y2)], South,C, DarkBlue)
                                                        | dir == South = ([(x1-1,y1+1),(x3,y3),(x2,y2+1),(x4+1,y4)], West,C, DarkBlue)
                                                        | dir == West = ([(x3,y3-1),(x3,y3),(x4,y4),(x4-1,y4)], North,C, DarkBlue)
                                                        | dir == North = ([(x1-1,y1+1),(x4,y4),(x3,y3),(x3+1,y3)], East,C, DarkBlue)

--If piece can rotate NEEDS TESTING------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets Coords from Piece
firstP :: Piece -> [Coord]
firstP (coords,dir,format,c) = coords

canRotate :: Game -> Bool
canRotate (b,s,p@(coords,d,f,c),np) = (all (\x -> x == Just Bg)  $ map (\x -> getBlock b x)  $ firstP (rotatePiece p))
            && (all (isMaybeNormal)  $ map (\x -> getBlock b x)  coords )

--Rotates Current Piece------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rotate :: Game -> Game
rotate g@(b,s,p,np) | canRotate g = (b,s,rotatePiece p,np)
                    | otherwise = g
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
posBoard :: Board -> Int -> [[(Int,Int)]] -> [[(Int,Int)]]
posBoard _ 10 res = res
posBoard (h:t) li [[]] = posBoard t (li+1) ([posInLine h (li,0)]) 
posBoard (h:t) li res = posBoard t (li+1) (res ++ [posInLine h (li,0)])

posInLine :: [Block] -> (Int,Int) -> [(Int,Int)]
posInLine [] (li,col)    = []
posInLine (h:t) (li,col) = (li,col) : posInLine t (li,col+1)