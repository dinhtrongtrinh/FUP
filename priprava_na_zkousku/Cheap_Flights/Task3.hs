module CheapFlights (cheapflight,Node,Cost,Edge,Graph,Path) where
import Data.List -- needed for sorting (see hints)

type Node = Int
type Cost = Float
type Edge = (Node,Node,Cost) 
type Graph = ([Node],[Edge]) 
type Path = [Node]

nodes :: [Node]
nodes = [1..6]

edges :: [Edge]
edges = [(1,2,0.5), (1,3,1.0), (2,3,2.0), (2,5,1.0), (3,4,4.0), (4,5,1.0)]

graph :: Graph
graph = (nodes,edges) 

cheapEdge :: (Path,Cost) -> [Node] -> [Edge] -> [(Path,Cost)] -> [(Path,Cost)]
cheapEdge _ _ [] acc = acc

cheapEdge routeList@(path,cost) covered ((fst,snd,costEdge):xs) acc =
    if fst == head path && snd `notElem` covered
    then cheapEdge routeList covered xs ((snd : path ,cost+costEdge) : acc)
    else 
        if snd == head path && fst `notElem` covered
        then cheapEdge routeList covered xs ((fst : path ,cost+costEdge) : acc)
        else cheapEdge routeList covered xs acc


lowcost (_,x) (_,y) | x < y = LT
                    | otherwise = GT


cheapflightCov :: [(Path,Cost)] -> Node -> Node -> Graph -> [Node] -> Maybe (Path,Cost)
cheapflightCov queue start end gr@(_, edges) covered
    | null queue = Nothing -- 1. Fronta je prázdná -> cesta neexistuje
    | otherwise =
        -- Nejdříve celou frontu seřadíme, abychom měli jistotu, že pracujeme s tím nejlevnějším
        let sortedQueue = sortOn snd queue
            (currPath, currCost) = head sortedQueue -- Vezmeme nejlevnější plán
            restQueue = tail sortedQueue            -- Zbytek fronty
            currAirport = head currPath             -- Kde zrovna jsme
        in
            -- 2. KONTROLA CÍLE
            if currAirport == end
            then Just (reverse currPath, currCost) -- Máme hotovo! Otočíme cestu do správného směru.
            
            -- 3. EXPANZE (Jedeme dál)
            else
                -- Vygenerujeme nové cesty. Jako covered předáme aktuální cestu, aby letadlo nelétalo v kruhu.
                let newPaths = cheapEdge (currPath, currCost) currPath edges []
                    -- Nové cesty spojíme se zbytkem fronty (operátor ++ je jako racketovský append)
                    updatedQueue = newPaths ++ restQueue
                in 
                    -- Zavoláme rekurzi s novou frontou
                    cheapflightCov updatedQueue start end gr covered



cheapflight :: Node -> Node -> Graph -> Maybe (Path,Cost)
cheapflight start end gr = cheapflightCov [([start],0.0)] start end gr []