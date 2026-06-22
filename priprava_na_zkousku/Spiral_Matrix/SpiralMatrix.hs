module SpiralMatrix ( spiralMatrix ) where
type Matrix = [[Int]]

addNum :: Matrix -> Matrix
addNum spiral =
    let len = (length spiral) + 2
        c = 4 * len - 4
    in map (\x -> map (\y -> y + c)x) spiral

buildMiddle :: [Int] -> Matrix -> [Int] -> Matrix
buildMiddle leftCol spiral rightCol = 
    zipWith3 (\l row r -> l : row ++ [r]) leftCol spiral rightCol

buildTB :: [Int] -> Matrix -> [Int] -> Matrix
buildTB top spiral left = 
    top : spiral ++ [left]


spiral3 :: Matrix
spiral3 = [[1,2,3],[8,9,4],[7,6,5]]

spiralMatrixWithAcc :: Int -> Int -> Matrix -> Matrix
spiralMatrixWithAcc n acc tempSpiral =
    if n == acc
    then tempSpiral
    else 
        let tempAcc = acc + 2
            add = addNum tempSpiral
            top   = [1 .. tempAcc]
            right = [tempAcc + 1 .. 2*tempAcc - 2]
            down  = [3*tempAcc - 2, 3*tempAcc - 3 .. 2*tempAcc - 1]
            left  = [4*tempAcc - 4, 4*tempAcc - 5 .. 3*tempAcc - 1]
            buildMid = buildMiddle left add right
            buildtd = buildTB top buildMid down
        in
            spiralMatrixWithAcc n tempAcc buildtd


spiralMatrix :: Int -> Matrix
spiralMatrix 1 = [[1]]
spiralMatrix 3 = spiral3
spiralMatrix n = 
    spiralMatrixWithAcc n 3 spiral3


-- your code goes here