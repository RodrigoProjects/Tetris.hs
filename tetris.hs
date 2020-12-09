type Game = (Board,Score)

type Board = [[Piece]]

type Score = Int

data Piece = Bg | Normal | Prev | Border deriving (Show,Eq) 

--Score
--n Lines destroyed -> 2^n pts
--

--Initial Board
initialBoard :: Board
initialBoard = createBoard 10 20 


--Creates Board
createBoard :: Int -> Int -> Board
createBoard li col = createAux li col li
                        where createAux :: Int -> Int -> Int -> Board
                              createAux 0 col ini = []
                              createAux li col ini | li==ini || li==1 = replicate col Border : createAux (li-1) col ini
                                                   | otherwise =  ([Border] ++ replicate (col-2) Bg ++ [Border]): createAux (li-1) col ini
                             

--Replace Piece in Board
replace :: Board -> Int -> Int -> Piece -> Board
replace [] _ _ _ = []
replace l@(h:t) x y piece | x<0 || y<0 || (length h)<=x ||  (length l<=y) = l
                          | y>0 = h : replace t x (y-1) piece
                          | y==0 = replaceAux h x piece : t


replaceAux :: [Piece] -> Int -> Piece -> [Piece]
replaceAux (h:t) 0 p = p:t
replaceAux (h:t) x p = h : replaceAux t (x-1) p


--Check if Line is full (Border [Normal...,Normal] Border)
checkFullLine :: [Piece] -> Bool
checkFullLine l = foldl (&&) True (map (\x -> x==Normal) ((init . tail) l))






posBoard :: Board -> Int -> [[(Int,Int)]] -> [[(Int,Int)]]
posBoard _ 10 res = res
posBoard (h:t) li [[]] = posBoard t (li+1) ([posInLine h (li,0)]) 
posBoard (h:t) li res = posBoard t (li+1) (res ++ [posInLine h (li,0)])

posInLine :: [Piece] -> (Int,Int) -> [(Int,Int)]
posInLine [] (li,col)    = []
posInLine (h:t) (li,col) = (li,col) : posInLine t (li,col+1)