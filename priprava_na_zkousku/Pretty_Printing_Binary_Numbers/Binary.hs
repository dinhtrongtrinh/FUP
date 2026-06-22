import Data.List (intercalate)

zero :: [String]
zero = [".##.",
        "#..#",
        "#..#",
        ".##."]

one :: [String]
one =  ["...#",
        "..##",
        "...#",
        "...#"]

numToBinary :: Int -> String
numToBinary 0 = "0"
numToBinary n = reverse (go n)
    where
        go 0 = ""
        go x = (if x `mod` 2 == 1 then '1' else '0') : go (x `div` 2)

buildOneLayer :: String -> Int -> String
buildOneLayer bin acc =
    -- Teprve TADY vezmeme ten seznam kousků a spojíme ho tečkami do jednoho Stringu!
    intercalate "." kouskyRadek
  where
    -- kouskyRadek bude typu [String]
    kouskyRadek = case acc of 
        0 -> foldr (\x zbytek -> (if x == '1' then one !! 0 else zero !! 0) : zbytek) [] bin 
        1 -> foldr (\x zbytek -> (if x == '1' then one !! 1 else zero !! 1) : zbytek) [] bin 
        2 -> foldr (\x zbytek -> (if x == '1' then one !! 2 else zero !! 2) : zbytek) [] bin 
        3 -> foldr (\x zbytek -> (if x == '1' then one !! 3 else zero !! 3) : zbytek) [] bin 
        _ -> []



buildImg :: String -> Int -> [String] -> [String]
buildImg bin acc tempImg= 
    if acc == 4 
    then reverse tempImg
    else buildImg bin (acc + 1) (buildOneLayer bin acc : tempImg)


        
main :: IO ()
main = do
    putStrLn "Enter integer: "
    inputStr <- getLine
    let n = read inputStr :: Int
    let binaryStr = numToBinary n
    let finalRows = buildImg binaryStr 0 []
    -- Vytiskne všech 5 řádků mřížky na obrazovku
    mapM_ putStrLn finalRows
    