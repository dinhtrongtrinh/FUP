module Task4 (grid) where
import Data.Char
import Data.List

-- Funkce najde maximální souřadnice X a Y ze všech bodů v seznamu.
-- Využívá List Comprehension k vytvoření seznamu všech X a všech Y a pak z nich vezme maximum.
-- Vrací fiktivní bod ('_', maxX, maxY), podle kterého se určí rozměr mapy.
largestPoint :: [(Char,Int,Int)] -> (Char, Int, Int)
largestPoint points = 
    let allX = [x | (_,x,_) <- points] -- Vytáhne všechna X
        allY = [y | (_,_,y) <- points] -- Vytáhne všechna Y
    in ('_', maximum allX, maximum allY)

-- Vypočítá manhattanskou vzdálenost mezi dvěma body.
-- Vzorec: |x1 - x2| + |y1 - y2|
calcDis :: (Char, Int, Int) -> (Char, Int, Int) -> Int
calcDis (_,x1,y1) (_,x2,y2) = 
    abs(x1 - x2) + abs(y1 - y2)

-- Pro zadaný cílový bod (target) najde v seznamu 'points' ten nejbližší.
-- Využívá funkci foldl1, která projde seznam zleva doprava a porovnává 
-- aktuální nejlepší výsledek (acc) s novým prvkem (curr).
closestPoint :: (Char, Int, Int) -> [(Char,Int,Int)] -> (Char, Int, Int)
closestPoint target points = foldl1 compare points
    where
        -- Vnitřní pomocná funkce pro porovnání dvou bodů
        compare acc@(char,x,y) curr =
            -- 1. Pokud je 'curr' blíž než náš dosavadní favorit 'acc', stává se novým favoritem
            if calcDis target curr < calcDis target acc
            then curr
            -- 2. Pokud mají stejnou vzdálenost, nastává shoda (sporné území) -> změníme znak na tečku '.'
            else if calcDis target curr == calcDis target acc
                then ('.',x,y)
                -- 3. Pokud je 'curr' dál, necháváme si starý 'acc'
                else acc

-- Rekurzivní funkce, která kontroluje, zda na zadaných souřadnicích (x1, y1)
-- nestojí přímo jeden z generujících bodů. 
-- Pokud seznam projde a nic nenajde, vrátí '*', což značí "prázdné místo".
matchPoint :: [(Char,Int,Int)] -> (Int, Int) -> Char
matchPoint points target@(x1,y1) = 
    if null points
    then '*' -- Žádný bod na těchto souřadnicích neleží
    else 
        let (char,x,y) = head points -- Rozbalíme první bod v seznamu
        in
            -- Pokud se souřadnice shodují, vrátíme jeho znak
            if (x == x1) && (y == y1)
            then char
            -- Jinak rekurzivně pokračujeme se zbytkem seznamu (tail)
            else matchPoint (tail points) target

-- Hlavní funkce, která vygeneruje mřížku jako seznam řetězců (2D pole znaků).
grid :: [(Char,Int,Int)] -> [[Char]]
grid points =
    let (_,maxX,maxY) = largestPoint points
        h = maxY -- Maximální X určuje výšku (počet řádků)
        w = maxX -- Maximální Y určuje šířku (počet sloupců)
    -- Vnořené List Comprehension generuje 2D mřížku (řádky 'y' a sloupce 'x')
    -- Pozor: Generuje se rozsah od 0 do h+1 a w+1 (přidává ohraničení o velikosti 1 buňky)
    in [[
        let matched = matchPoint points (x,y)
            (closestChar,_,_) = closestPoint ('1',x,y) points
        in
            -- Pokud na daném místě nestojí přímo hlavní bod (matchPoint vrátil '*')
            if matched == '*'
            -- ...tak zjistíme nejbližší bod a vykreslíme jeho malé písmeno (toLower)
            then toLower closestChar
            -- Pokud tam hlavní bod stojí, vykreslíme ho přímo jako velké písmeno
            else closestChar
        | x <- [0..w]] | y <- [0..h]] -- Cyklus přes X (vnitřní) a Y (vnější)

-- Testovací data
points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]