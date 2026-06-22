-- Načte od uživatele řádek čísel (např. "5 3 8 1").

-- Převede je na seznam [Int].

-- Vytvoří z nich Binární vyhledávací strom (BST).

-- Spočítá výšku tohoto stromu.

-- Vypíše výsledek uživateli.

import Data.List (group)
import Data.Char (isDigit, isUpper)

data Tree = NotTree | Node Int Tree Tree deriving Show
--1.
stringToInt :: String -> Int
stringToInt a = read a
stringToListInt :: String -> [Int]
stringToListInt ""= []
stringToListInt xs = map stringToInt (words xs)


--2.
insertValue :: Int -> Tree -> Tree
insertValue x NotTree = Node x NotTree NotTree
insertValue x (Node v l r)
    | x < v = Node v (insertValue x l) r
    |otherwise = Node v l (insertValue x r) 

listIntToBST :: [Int] -> Tree
listIntToBST [] = NotTree
listIntToBST xs = foldl (\strom x -> insertValue x strom) NotTree xs

--3.
heightBst :: Tree -> Int
heightBst NotTree = 0
heightBst (Node hodnota levy pravy) = 1 + (max (heightBst levy) (heightBst pravy)) 

main :: IO()
main = do
    putStrLn "Zadej string hodnot:"
    vstup <- getLine

    let funcAnswer = heightBst . listIntToBST . stringToListInt 
        answer = funcAnswer vstup
    putStrLn $ "Vyska: " ++ show answer
    
