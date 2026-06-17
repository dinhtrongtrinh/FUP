module Task4 ( Tree (..), buildHeap) where

-- Definice datového typu pro strom
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq)

-- Pomocná funkce pro hezké zobrazení stromu v konzoli
tostr :: (Show a) => Tree a -> Int -> String
tostr Leaf d = ""
tostr (Node x l r) d = tostr l (d+1) ++ concat (replicate d "---") ++ show x ++ "\n" ++ (tostr r (d+1))

instance (Show a) => Show (Tree a) where
    show tree = tostr tree 0

-- Tady bude tvoje práce (zatím provizorní prázdná funkce, aby šel soubor načíst)
mindepth :: Tree a -> Int
mindepth Leaf = 0
mindepth (Node val left right) = 
    1 + (min (mindepth left) (mindepth right))

insertAtEmpty :: (Eq a, Ord a) => a -> Tree a -> Tree a
insertAtEmpty value Leaf = Node value Leaf Leaf
insertAtEmpty value (Node x left right) = 
    if (mindepth left) < (mindepth right)
    then Node x (insertAtEmpty value left) right
    else Node x left (insertAtEmpty value right)

nodeVal :: Tree a -> a
nodeVal (Node v _ _) = v

enforceHeap :: (Eq a, Ord a) => Tree a -> Tree a
enforceHeap Leaf = Leaf
enforceHeap (Node value left right) =
    let leftHeap  = enforceHeap left
        rightHeap = enforceHeap right
    in 
        if (leftHeap /= Leaf) && (value < (nodeVal leftHeap))
        then 
            case leftHeap of
                Node lv ll lr -> Node lv (Node value ll lr) rightHeap
        else
            if (rightHeap /= Leaf) && (value < (nodeVal rightHeap))
            then
                case rightHeap of 
                    Node rv rl rr -> Node rv leftHeap (Node value rl rr)
            else
                Node value leftHeap rightHeap

buildHeap :: (Eq a, Ord a) => [a] -> Tree a
buildHeap [] = Leaf
buildHeap (x:xs) = enforceHeap (insertAtEmpty x (buildHeap xs))
