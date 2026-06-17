module Skyscrapers (bestView) where
import Data.List (transpose)

rot :: [[Int]] -> [[Int]]
rot [[]] = [[]]
rot grid = transpose (reverse grid)

checkRoofLine :: [Int] -> Int -> Int
checkRoofLine (x:xs) total = 
    if null xs
    then total
    else if x < head xs
        then checkRoofLine xs (total + 1) 
        else total

countRoofs :: [[Int]] -> Int
countRoofs grid =
    foldl (+) 0 (map (\row -> checkRoofLine row 1) grid)

findBest :: [(Char, Int)] -> (Char, Int) -> (Char, Int)
findBest [] temp = temp
findBest ((xDir, xCount):xs) temp@(_, tempCount) =
    if tempCount < xCount
    then findBest xs (xDir, xCount)
    else findBest xs temp
    

bestView :: [[Int]] -> (Char, Int)
bestView city = 
    let w = countRoofs city
        s = countRoofs (rot city)
        e = countRoofs (rot (rot city))
        n = countRoofs (rot (rot(rot city)))

    in findBest (zip ['W', 'S', 'E', 'N'] [w,s,e,n]) ('L', 0)

city :: [[Int]]
city = [[3, 3, 3],
        [1, 2, 3],
        [1, 2, 3]]

-- Město 1: Ploché město (všechny budovy stejně vysoké)
-- Z každé strany by měla být vidět vždy jen ta první budova na kraji.
-- 3 řádky/sloupce * 1 budova = 3 budovy z každého směru. Vrací první nalezené maximum ('W').
city1 :: [[Int]]
city1 = [[5, 5, 5],
         [5, 5, 5],
         [5, 5, 5]]
-- Očekávaný výsledek: ('W', 3)


-- Město 2: Rostoucí schody směrem na východ a na jih
-- Ze západu (W) a severu (N) uvidíš úplně všechny budovy (3 * 3 = 9).
-- Z východu (E) a jihu (S) uvidíš jen ty nejbližší krajní (3).
city2 :: [[Int]]
city2 = [[1, 2, 3],
         [2, 3, 4],
         [3, 4, 5]]
-- Očekávaný výsledek: ('W', 9) -- (případně 'N', 9 podle toho, jak řadíš shodu)


-- Město 3: Obklíčené centrum
-- Uprostřed je obří mrakodrap (9), okolo jsou malé budovy.
-- Ze všech stran uvidíš krajní budovy + tu devítku uprostřed.
-- Výjimečný je Západ (W), kde ve druhém řádku uvidíš postupně: 1, 2 a pak 9 (celkem 3 budovy v řádku).
city3 :: [[Int]]
city3 = [[3, 1, 3],
         [1, 2, 9],
         [3, 1, 3]]
-- Očekávaný výsledek: ('W', 7)


-- Město 4: Skutečný chaos (komplexnější test)
-- Tohle město simuluje reálné zadání s různými výškami.
city4 :: [[Int]]
city4 = [[3, 0, 3, 7, 3],
         [2, 5, 5, 1, 2],
         [6, 5, 3, 3, 2],
         [3, 3, 5, 4, 9],
         [3, 5, 3, 9, 0]]
-- Očekávaný výsledek: ('W', 11)