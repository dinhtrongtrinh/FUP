module Automata (accepts, lwords, Transition (..), Automaton (..)) where

data Transition a b = Tr a b a deriving (Show, Eq)
data Automaton a b = NFA [(Transition a b)] a [a] deriving (Show, Eq)

findNextState :: (Eq a, Eq b) => a -> [(Transition a b)] -> b -> [a]
findNextState currState trans name = go trans []
    where
        go [] acc = acc
        go (Tr from sym to : xs) acc =
            if from == currState && name == sym
            then go xs (to : acc)
            else go xs acc

currStateToNextState :: (Eq a, Eq b) => [(Transition a b)] -> [a] -> b -> [a]
currStateToNextState trans listCurrState name = go listCurrState []
    where 
        go [] acc = acc
        go (x:xs) acc = 
            go xs (acc ++ findNextState x trans name)
    
checkIfInList :: Eq a => [a] -> [a] -> Bool
checkIfInList [] _ = False
checkIfInList (x:xs) listEnd =
    if x `elem` listEnd
    then True
    else checkIfInList xs listEnd

accepts :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
accepts (NFA trans initState listEndState) listMoves = go listMoves [initState]
    where
        go [] acc = 
            if checkIfInList acc listEndState
            then True
            else False
        go (x:xs) acc = 
            go xs (currStateToNextState trans acc x)


genWords :: [b] -> Int -> [[b]]
genWords _ 0 = [[]]
genWords alphabet n = [c : w | c <- alphabet, w <- genWords alphabet (n - 1)]

lwords :: (Eq a, Eq b) => [b] -> Automaton a b -> Int -> [[b]]
lwords alphabet automaton n = 
    let allWords = genWords alphabet n  -- (případně replicateM n alphabet)
    in filter (\word -> accepts automaton word) allWords

nfa::Automaton Int Char
nfa = NFA [Tr 1 'a' 2,
           Tr 2 'b' 2,
           Tr 1 'a' 3,
           Tr 3 'b' 4,
           Tr 4 'a' 3,
           Tr 2 'a' 4]
           1
           [2,3]