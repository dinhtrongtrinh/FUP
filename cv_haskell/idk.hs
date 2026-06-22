import Data.List (group)
import Data.Char (isDigit, isUpper)

doublePositive :: [Int] -> [Int]

--if list is empty
doublePositive [] = []
doublePositive seznam = map (*2) (filter (\x -> x > 0) seznam)  

allGreaterThanFive :: [Int] -> Bool

allGreaterThanFive [] = False
allGreaterThanFive [a] = a > 5
allGreaterThanFive (x:xs) = x > 5 && allGreaterThanFive xs

mergeConnect :: [Int] -> [Int] -> [Int]

-- if one of the list is empty
mergeConnect(x:xs)[] = (x:xs)
mergeConnect[](x:xs) = (x:xs)
mergeConnect[][] = []

--the algorithm
mergeConnect(x:xs)(y:ys) =
    if x <= y 
    then x : (mergeConnect xs (y:ys)) 
    else y : (mergeConnect (x:xs) ys)

mergeSort :: [Int] -> [Int]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let n = length xs `div` 2
        (levaPolovina, pravaPolovina) = splitAt n xs
    in mergeConnect (mergeSort levaPolovina) (mergeSort pravaPolovina)


averagesNiggabour :: [Float] -> [Float]

averagesNiggabour [] = []
averagesNiggabour [a] = [a]
averagesNiggabour xs = map (/2) (zipWith (+) xs (tail xs))

averageInList :: [Float] -> Float

averageInList [] = 0
averageInList [a] = a
averageInList xs = 
    let sizeList = length xs
    in (sum xs) / 2

lenghtEachIndex :: [String] ->[Int]
lenghtEachIndex [] = []
lenghtEachIndex xs = map (length) xs

compress :: String -> [(Int, Char)]
compress [] = []
compress xs = 
    let dividedString = group xs
        letterEachGroup = map (head) dividedString
        lenghtEachGroup = lenghtEachIndex dividedString
    in zip lenghtEachGroup letterEachGroup

bezpecnaHlava :: [Int] -> Maybe Int
bezpecnaHlava []    = Nothing
bezpecnaHlava (x:xs) = Just x 


data Strom = Prazdny 
           | Uzel Int Strom Strom
           deriving Show

sectiStrom :: Strom -> Int

sectiStrom Prazdny = 0
sectiStrom (Uzel hodnota levy pravy) = hodnota + sectiStrom levy + sectiStrom pravy

vyska :: Strom -> Int 

vyska Prazdny = 0
vyska (Uzel hodnota levy pravy) = 1 + (max (vyska levy) (vyska pravy)) 

mapStrom :: (Int -> Int) -> Strom -> Strom
mapStrom f Prazdny = Prazdny
mapStrom f (Uzel x l p) = Uzel (f x) (mapStrom f l) (mapStrom f p)


seznamPrvku :: Strom -> [Int]
seznamPrvku Prazdny = []
seznamPrvku (Uzel hodnota leva prava) = 
    let listLeve = seznamPrvku leva
        listPrave = seznamPrvku prava
    in [hodnota] ++ listLeve ++ listPrave



isStrong :: String -> Bool
isStrong heslo = ((length heslo) >= 8) && (any (isDigit) heslo) && (any (isUpper) heslo)

vratJenSudyNasobky3 :: [Int] -> [Int]
vratJenSudyNasobky3 xs = [ x*3 | x <- xs, even x]

duplikujPrvni :: [Int] -> [Int]
duplikujPrvni s@(x:xs) = [x] ++ s

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = 
    if y == 0 then Nothing
    else Just (x `div` y)


data Shape = Circle Float | Rect Float Float
isLarge :: Shape -> Bool
isLarge (Circle r) = 100 < r * r * pi
isLarge (Rect a b) = 100 < a * b

main :: IO ()
main = do
    putStrLn "Zadej číslo (nebo 'stop'):"
    -- Spustíme "smyčku" s počáteční hodnotou 0
    smycka 0

-- Pomocná funkce, která nese stav (aktualniPocet)
smycka :: Int -> IO ()
smycka aktualniPocet = do
    vstup <- getLine
    
    if vstup == "stop"
        then do 
            putStrLn $ "Tvůj konečný součet je: " ++ show aktualniPocet
        else do
            let cislo = read vstup :: Int
                suma = cislo + aktualniPocet
            
            putStrLn $ "Prubezny suma: " ++ show suma
            putStrLn $ "Napiš další:"
            
        
            smycka suma