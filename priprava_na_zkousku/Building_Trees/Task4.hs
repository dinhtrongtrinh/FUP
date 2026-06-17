module Task4 (addEdge, buildTree, Tree(..)) where
import Data.List

data Tree a = Leaf { val :: a } 
            | Node { val :: a,
                     kids :: [Tree a] } deriving (Eq,Show) 

type Edge a = (a,a)

addEdge :: Eq a => Tree a -> Edge a -> Tree a
addEdge (Leaf x) (from, to) =
    if x == from
    then Node { val = x, kids = [Leaf to] }
    else Leaf x
addEdge (Node x children) (from, to) = 
    if x == from
    then Node {val = x, kids = (Leaf to) : children}
    else Node { val = x, kids = map (\k -> addEdge k (from, to)) children }
    

buildTree :: Ord a => Tree a -> [Edge a] -> Tree a
buildTree strom hrany = foldl addEdge strom hrany
-- implement me!