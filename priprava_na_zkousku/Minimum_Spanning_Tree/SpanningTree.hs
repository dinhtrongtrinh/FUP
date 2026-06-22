module SpanningTree (spanningTree, Graph (..), Edge (..)) where
import Data.List -- for sortOn

data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)

data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show

-- your code goes here
boudaryEdge :: Eq a => Edge a b -> [a] -> Bool
boudaryEdge edge covered = 
    if ((u edge) `elem` covered) && ((v edge) `elem` covered)
    then False
    else if ((u edge) `elem` covered) || ((v edge) `elem` covered)
        then True 
        else False



findNextEdge :: (Eq a, Ord b) => [Edge a b] -> [a] -> Edge a b  
findNextEdge edgeList covered =
    let sortEdgeList = sortOn weight edgeList
        currEdge = head (sortOn weight edgeList)
    in
        if boudaryEdge currEdge covered
        then currEdge
        else findNextEdge (tail sortEdgeList) covered

whichEdgeToAdd :: Eq a => Edge a b -> [a] -> a
whichEdgeToAdd edge covered =
    if (u edge) `notElem` covered
    then (u edge)
    else (v edge)


spanningTree :: (Eq a, Ord b) => Graph a b -> [Edge a b]
spanningTree graph = go graph [(head(nodes graph))] []
    where 
        go graph covered treeEdge=
            if (length (nodes graph)) == (length covered)
            then treeEdge
            else 
                let newEdge = findNextEdge (edges graph) covered
                    edgeToAdd = whichEdgeToAdd (findNextEdge (edges graph) covered) covered
                in go graph (edgeToAdd : covered) (newEdge : treeEdge)
gr :: Graph Char Int
gr = Graph{ nodes = ['A'..'F'],
            edges = [Edge 'A' 'B' 1,
                     Edge 'D' 'E' 4,
                     Edge 'E' 'F' 7,
                     Edge 'A' 'D' 5,
                     Edge 'B' 'E' 2,
                     Edge 'C' 'F' 5,
                     Edge 'D' 'B' 6,
                     Edge 'E' 'C' 4,
                     Edge 'A' 'E' 3] }

