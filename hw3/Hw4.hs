module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

varP :: Parser Symbol
varP = some alphaNum

exprP :: Parser Expr
exprP = varExpr <|> lambdaExpr <|> appExpr
  where
    varExpr = Var <$> varP

    lambdaExpr = do
        char '('
        string "\\"
        v <- varP
        char '.'
        e <- exprP
        char ')'
        return (Lambda v e)

    appExpr = do
        char '('
        e1 <- exprP
        sep
        e2 <- exprP
        char ')'
        return (App e1 e2)

defP :: Parser (Symbol, Expr)
defP = do
    v <- varP
    sep
    string ":="
    sep
    e <- exprP
    return (v, e)

progP :: Parser ([(Symbol, Expr)], Expr)
progP = do
    defs <- many (defP <* sep)
    mainExpr <- exprP
    many (sat isSpace)
    return (defs, mainExpr)

dumbSubst :: Symbol -> Expr -> Expr -> Expr
dumbSubst x e (Var y)
    | x == y    = e
    | otherwise = Var y
dumbSubst x e (App e1 e2) = 
    App (dumbSubst x e e1) (dumbSubst x e e2)
dumbSubst x e (Lambda y e')
    | x == y    = Lambda y e'
    | otherwise = Lambda y (dumbSubst x e e')

resolveAll :: [(Symbol, Expr)] -> Expr -> Expr
resolveAll [] mainE = mainE
resolveAll ((v, e) : defs) mainE =
    let defs' = map (\(v', e') -> (v', dumbSubst v e e')) defs
        mainE' = dumbSubst v e mainE
    in resolveAll defs' mainE'

readPrg :: String -> Maybe Expr
readPrg inp = case parse progP inp of
    Just ((defs, mainE), "") -> Just (resolveAll defs mainE)
    _                        -> Nothing