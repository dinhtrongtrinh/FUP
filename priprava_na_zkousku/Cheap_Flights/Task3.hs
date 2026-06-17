module Task3 (cheapflight, Node, Cost, Edge, Graph, Path) where
import Data.List (sortBy)

type Node = Int
type Cost = Float
type Edge = (Node, Node, Cost) 
type Graph = ([Node], [Edge]) 
type Path = [Node]

-- 1. POMOCNÁ FUNKCE: Vyhledání sousedních hran pro daný uzel
findNegh :: Node -> [Edge] -> [Edge]
findNegh node edges = 
    filter (\(node1, node2, cost) -> node1 == node || node2 == node) edges

-- 2. POMOCNÁ FUNKCE: Seřazení fronty podle ceny vzestupně
seradFrontu :: [(Cost, Path)] -> [(Cost, Path)]
seradFrontu unsortedList = 
    sortBy (\(cost1, path1) (cost2, path2) -> compare cost1 cost2) unsortedList

-- 3. HLAVNÍ FUNKCE
cheapflight :: Node -> Node -> Graph -> Maybe (Path, Cost)
cheapflight a b (allNodes, allEdges) = search (listNaZacatku)
  where
    -- Výchozí stav fronty: cena 0, cesta obsahuje jen startovní uzel 'a'
    listNaZacatku = [(0.0, [a])]

    -- Rekurzivní vyhledávací funkce
    search :: [(Cost, Path)] -> Maybe (Path, Cost)
    search [] = Nothing -- Konec: fronta je prázdná, cesta neexistuje
    search ((aktualniCena, aktualniCesta):xs) =
        let aktualniUzel = head aktualniCesta
        in if aktualniUzel == b
           then Just (reverse aktualniCesta, aktualniCena) -- Konec: našli jsme cíl!
           else
               -- 1. KROK: Najdeme sousedy a vytvoříme nové cesty (přes map)
               let noveCesty = map (\(u1, u2, cenaHrany) -> 
                                    let soused = if u1 == aktualniUzel then u2 else u1
                                    in (aktualniCena + cenaHrany, soused : aktualniCesta)
                                   ) (findNegh aktualniUzel allEdges)
                   
                   -- 2. KROK: Ochrana proti zacyklení (přes filter)
                   platneCesty = filter (\(novaCena, novaCesta) -> 
                                         let soused = head novaCesta
                                         in not (soused `elem` aktualniCesta)
                                        ) noveCesty
                   
                   -- 3. KROK: Spojení se zbytkem fronty 'xs' a seřazení
                   novaFronta = seradFrontu (xs ++ platneCesty)
               
               -- 4. KROK: Skok do dalšího kola rekurze
               in search novaFronta