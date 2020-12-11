module Tetris where

type Game = (Board,Score)

type Board = [[Piece]]

type Score = Int

data Piece = Bg | Normal | Prev | Border deriving (Show,Eq) 


--Score
--n Lines destroyed -> 2^n pts
--

-- Test board
testBoard :: Board
testBoard = replicate 10 Border : (replicate 8 ( [Border] ++ replicate 8 Normal ++ [Border])) ++ [replicate 10 Border]

--Initial Board
initialBoard :: Board
initialBoard = createBoard 10 20 


--Creates Board
createBoard :: Int -> Int -> Board
createBoard li col = createAux li col li
                        where createAux :: Int -> Int -> Int -> Board
                              createAux 0 _ _ = []
                              createAux li col ini | li==ini || li==1 = replicate col Border : createAux (li-1) col ini
                                                   | otherwise =  ([Border] ++ replicate (col-2) Bg ++ [Border]): createAux (li-1) col ini


-- Checks if we got a full line in the board and updates the score and the board.
updateGame :: Game -> Game
updateGame (board, score) = (removeFullLines board, newScore)
                          where newScore = case getScore board of 
                                                1 -> score
                                                n -> 2^n + score


-- Get the score of a game state.
getScore :: Board -> Score
getScore = (2^) . foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 

-- Remove the full lines of a board and add new empty ones.
removeFullLines :: Board -> Board
removeFullLines b =  head b : replicate fullLines ([Border] ++ replicate ( flip (-) 2 $ length $ head b) Bg ++ [Border]) ++ tail (filter (not . checkFullLine) b)
                  where fullLines = foldr (\ e1 e2 -> if checkFullLine e1 then 1 + e2 else 0 + e2) 0 b
                              

--Replace Piece in Board
replace :: Board -> Int -> Int -> Piece -> Board
replace [] _ _ _ = []
replace l@(h:t) x y piece | x<0 || y<0 || length h <=x ||  (length l<=y) = l
                          | y>0 = h : replace t x (y-1) piece
                          | y==0 = replaceAux h x piece : t


replaceAux :: [Piece] -> Int -> Piece -> [Piece]
replaceAux (h:t) 0 p = p:t
replaceAux (h:t) x p = h : replaceAux t (x-1) p


--Check if Line is full (Border [Normal...,Normal] Border)
checkFullLine :: [Piece] -> Bool
checkFullLine l = and $ (map (\x -> x==Normal) ((init . tail) l))





posBoard :: Board -> Int -> [[(Int,Int)]] -> [[(Int,Int)]]
posBoard _ 10 res = res
posBoard (h:t) li [[]] = posBoard t (li+1) ([posInLine h (li,0)]) 
posBoard (h:t) li res = posBoard t (li+1) (res ++ [posInLine h (li,0)])

posInLine :: [Piece] -> (Int,Int) -> [(Int,Int)]
posInLine [] (li,col)    = []
posInLine (h:t) (li,col) = (li,col) : posInLine t (li,col+1)