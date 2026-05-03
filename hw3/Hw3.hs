module Hw3 where

type Symbol = String

data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq


instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Lambda x e) = "(\\" ++ x ++ "." ++ show e ++ ")"

freeVars :: Expr -> [Symbol]
freeVars (Var x) = [x]
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Lambda x e) = filter (/= x) (freeVars e)

subst :: Symbol -> Expr -> Expr -> Int -> (Expr, Int)
subst x e (Var y) c
    | x == y    = (e, c)         
    | otherwise = (Var y, c)     

subst x e (App e1 e2) c =
    let (e1', c1) = subst x e e1 c
        (e2', c2) = subst x e e2 c1
    in (App e1' e2', c2)

subst x e (Lambda y e') c
    | x == y = (Lambda y e', c)  
    | y `elem` freeVars e = 
        let z = "a" ++ show c                            
            (eAlpha, c1) = subst y (Var z) e' (c + 1)     
            (eSubst, c2) = subst x e eAlpha c1            
        in (Lambda z eSubst, c2)
    | otherwise =

        let (eSubst, c1) = subst x e e' c
        in (Lambda y eSubst, c1)

evalStep :: Expr -> Int -> Maybe (Expr, Int)
evalStep (Var _) _ = Nothing
evalStep (App (Lambda x body) arg) c = 

    Just (subst x arg body c)
evalStep (App e1 e2) c =
    case evalStep e1 c of
        Just (e1', c1) -> Just (App e1' e2, c1)
        Nothing -> 
        
            case evalStep e2 c of
                Just (e2', c2) -> Just (App e1 e2', c2)
                Nothing -> Nothing
evalStep (Lambda x body) c =
    
    case evalStep body c of
        Just (body', c1) -> Just (Lambda x body', c1)
        Nothing -> Nothing

evalLoop :: Expr -> Int -> Expr
evalLoop expr c =
    case evalStep expr c of
        Just (expr', c') -> evalLoop expr' c'  
        Nothing -> expr                        


eval :: Expr -> Expr
eval expr = evalLoop expr 0