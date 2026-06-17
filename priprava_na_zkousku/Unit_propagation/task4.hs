module UnitPropagation ( propagateUnits, Literal (..) ) where
import Data.List -- for delete, nub functions


type Variable = String
data Literal = Neg { variable :: Variable }
             | Pos { variable :: Variable } deriving (Eq, Ord)

type Clause = [Literal]

instance Show Literal where
  show (Neg x) = "-" ++ x
  show (Pos x) = x

findUnit :: [Clause] -> Maybe Literal
findUnit [] = Nothing
findUnit (x:xs) = 
    if length x == 1
    then Just (head x)
    else findUnit xs

negateLiteral :: Literal -> Literal
negateLiteral (Pos var) = Neg var
negateLiteral (Neg var) = Pos var

findLiteral :: Literal -> Clause -> Maybe Literal
findLiteral _ [] = Nothing
findLiteral lite (x:xs) = 
    if lite == x
    then Just x
    else if lite == negateLiteral x
        then Just x
        else findLiteral lite xs

cleanFormula :: Literal -> [Clause] -> [Clause]
cleanFormula lite listClause = 
    -- 1. Krok: Vyfiltruj (zahod) všechny klauzule, které obsahují tvůj unit
    let prezivsiKlauzule = filter (\clause -> not (elem lite clause)) listClause
    -- 2. Krok: Na ty, co přežily, pusť map, který z nich vymaže ty negace
    in map (filter (/= negateLiteral lite)) prezivsiKlauzule




propagateUnits :: [Clause] -> [Clause]
propagateUnits formula =
    case findUnit formula of
        Nothing -> 
            -- Žádný unit už v seznamu není -> KONEC! Vrátíme hotový výsledek.
            formula
            
        Just unit -> 
            -- Našli jsme unit! Provedeme jedno kolo čistky:
            let cleanFormula = [ filter (/= negateLiteral unit) clause 
                               | clause <- formula
                               , not (elem unit clause) ]
            -- S touto novou vyčištěnou formulí skočíme do dalšího kola (rekurze)
            in propagateUnits cleanFormula
-- your code goes here
-- 1 9 , 13 10 , 15 4 