-- for converting ints to chars
import Data.Char (intToDigit)

-- for testing
test_board = ["..."
             ,".**"
             ,"..."]

isValid :: [String] -> (Int,Int) -> Bool
isValid board (x,y) =
    let boardH = length board
        boardW = length (head board)
    in ((x >= 0 && x < boardW) && (y >= 0 && y < boardH))
        
combination :: [(Int, Int)]
combination = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]

getNegh :: [String] -> (Int, Int) -> [(Int, Int)]
getNegh board (currx, curry) = 
    filter (isValid board) (map (\(dx, dy) -> (currx + dx, curry + dy)) combination)

countMines :: [String] -> [(Int,Int)] -> Int
countMines board validNegh =
    length (filter (== '*') (map (\(x,y) -> (board !! y) !! x) validNegh))

charRule :: Char -> Int -> Char
charRule '*' _ = '*'
charRule _ 0 = '.'
charRule _ x = intToDigit x

sweep :: [String] -> [String]
sweep board = 
    let h = length board
        w = length (head board)
    in [ [charRule ((board !! y) !! x) (countMines board (getNegh board (x,y)))|x <- [0..w-1]]| y <- [0..h-1]]


nactiRadky :: Int -> IO [String]
nactiRadky 0 = return []
nactiRadky n = do
    radek <- getLine
    zbytek <- nactiRadky (n-1)
    return (radek : zbytek)


readInput :: IO [String]
readInput = do
    cisloRadek <- getLine
    let n = read cisloRadek :: Int
    nactiRadky n
    

main = do
  lines <- readInput
  putStrLn "\nSweep Result:"
  let sw = sweep lines
  mapM_ putStrLn sw