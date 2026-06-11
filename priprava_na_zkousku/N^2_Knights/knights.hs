import Data.List

data Piece = Nil | Knight deriving (Eq)
combination :: [(Int,Int)]
combination = [(x,y)| x <- [-2,-1,1,2], y <- [-2,-1,1,2], x /= y, -x /= y]

isValid :: [[Piece]] -> (Int,Int) -> Bool
isValid board (x,y) =
    let boardH = length board
        boardW = length (head board)
    in ((x >= 0 && x < boardW) && (y >= 0 && y < boardH))
        

getNegh :: [[Piece]] -> (Int, Int) -> [(Int, Int)]
getNegh board (currx, curry) = 
    filter (isValid board) (map (\(dx, dy) -> (currx + dx, curry + dy)) combination)

howManyAttck :: [[Piece]] -> [(Int,Int)] -> Int
howManyAttck board validNegh =
    length (filter (== Knight) (map (\(x,y) -> (board !! y) !! x) validNegh))

isKnight :: Int -> Int -> [[Piece]] -> Bool
isKnight x y board = 
    if ((board !! y) !! x) == Knight
    then True
    else False

is_valid :: [[Piece]] -> Bool
is_valid board = 
    let h = length board
        w = length (head board)
        -- Vygenerujeme seznam všech (x,y), kde sedí jezdec a zároveň někoho ohrožuje
        badKnights = [(x,y) | x <- [0..w-1], 
                             y <- [0..h-1], 
                             isKnight x y board, 
                             howManyAttck board (getNegh board (x,y)) > 0]
    in null badKnights -- Funkce 'null' vrátí True, pokud je seznam prázdný


-- ... (všechny tvoje funkce zůstávají stejné) ...

-- Tuhle testovací desku si uložíme do konstanty, ať je to přehledné
testBoard :: [[Piece]]
testBoard = [[Nil,    Knight, Nil,    Nil],
             [Nil,    Nil,    Nil,    Nil],
             [Nil, Nil,    Nil,    Nil],
             [Nil,    Nil,    Knight, Nil]]

-- Hlavní spouštěcí funkce programu
main :: IO ()
main = print (is_valid testBoard)