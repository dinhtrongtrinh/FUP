import Data.List 
import Data.Char 

recursion :: IO()
recursion = do 
    line <- getLine
    if line == "stop"
    then do putStrLn ""
    else do putStrLn line
            recursion


main :: IO()
main = do
    recursion
    