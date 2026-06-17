module Task4 (grid) where
import Data.Char
import Data.List

largestPoint :: [(Char,Int,Int)] -> (Char, Int, Int)
largestPoint points = 
    let allX = [x | (_,x,_) <- points]
        allY = [y | (_,_,y) <- points]
    in ('_', maximum allX, maximum allY)

calcDis :: (Char, Int, Int) -> (Char, Int, Int) -> Int
calcDis (_,x1,y1) (_,x2,y2) = 
    abs(x1 - x2) + abs(y1 - y2)


closestPoint :: (Char, Int, Int) -> [(Char,Int,Int)] -> (Char, Int, Int)
closestPoint target points = foldl1 compare points
    where
        compare acc@(char,x,y) curr=
            if calcDis target curr < calcDis target acc
            then curr
            else if calcDis target curr == calcDis target acc
                then ('.',x,y)
                else acc

matchPoint :: [(Char,Int,Int)] -> (Int, Int) -> Char
matchPoint points target@(x1,y1) = 
    if null points
    then '*'
    else 
        let (char,x,y) = (head points)
        in
            if (x == x1) && (y == y1)
            then char
            else matchPoint (tail points) target
    



grid :: [(Char,Int,Int)] -> [[Char]]
grid points =
    let (_,maxX,maxY) = largestPoint points
        h = maxX
        w = maxY
    in [[
        let matched = matchPoint points (x,y)
            (closestChar,_,_) = closestPoint ('1',x,y) points
        in
            if matched == '*'
            then toLower closestChar
            else closestChar
        |x <- [0..w+1]]|  y <- [0..h+1]]

points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]


