module SquareCode ( encode ) where
import Data.Char
import Data.List

normalize :: String -> String
normalize str =
    let withAplha = filter isAlpha str
    in map toLower withAplha

squareUp :: String -> [String]
squareUp str =
    let len = length str
        c = ceiling (sqrt (fromIntegral len))
    in go str [] c
    where 
        go [] acc c = reverse acc
        go currStr acc c = 
            if (length currStr) <= c 
            then go (drop c currStr) ((take c currStr ++ [' '| _ <- [1..(c -(length currStr))]]) : acc) c
            else go (drop c currStr) (take c currStr : acc) c

encode :: String -> String
encode str = 
    let normal = normalize str
        squar = squareUp normal
        trans = transpose squar
    in unwords trans
-- your code goes here