module ConvexHull (convexHull)  where
import Data.List (sortOn)

getLowestPoint :: (Eq a, Ord a) => [(a,a)] -> (a,a)
getLowestPoint setPoints =
    let firstSort = sortOn snd setPoints
        onlyLowest = filter (\(_,y) -> y == snd (head firstSort)) firstSort
    in last (sortOn fst onlyLowest)

calcDegree :: RealFloat a => (a, a) -> (a, a) -> a
calcDegree start@(x0, y0) end@(x, y) =
    let dx = x - x0  
        dy = y - y0
    in atan2 dy dx

sortByDegree :: RealFloat a => (a, a) -> [(a,a)] -> [(a,a)]
sortByDegree start listPoints =
    start : sortOn (\x -> calcDegree start x) listPoints


convexHull :: RealFloat a => [(a,a)] -> [(a,a)]
convexHull listPoints =
    let lowestPoint = getLowestPoint listPoints
        sortDegreeList = sortByDegree lowestPoint listPoints
        startStack = take 2 sortDegreeList
        remainingPoints = drop 2 sortDegreeList
    in 
            go remainingPoints startStack
        where
            lowestPoint = getLowestPoint listPoints
            go [] convexList = reverse convexList
            go (currPoint@(x3,y3):as) convexList@((x1,y1):(x2,y2):us) = 
                -- OPRAVENÝ VZOREC KŘÍŽOVÉHO SOUČINU
                let answer = (x1 - x2) * (y3 - y2) - (y1 - y2) * (x3 - x2)
                in if answer > 0
                    then go as (currPoint : convexList)
                    else go (currPoint:as) ((x2,y2):us)
            -- ZÁCHRANNÝ SEZNAM: Pokud má zásobník méně než 2 body, prostě přidej bod
            go (currPoint:as) convexList = go as (currPoint : convexList)
                


points = [(-2.0, 3.0), ( 2.0, 2.0), (-1.0, 1.0),
          (-2.0,-1.5), ( 4.0,-1.0), ( 1.0,-3.0)]

idk = sortByDegree (getLowestPoint points) points