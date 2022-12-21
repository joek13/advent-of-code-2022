module Main where

type Grid a = [[a]]

col :: Int -> Grid a -> [a]
-- gets the jth column of a grid
col j = map (!! j)

row :: Int -> Grid a -> [a]
-- gets the ith row of a grid
row i g = g !! i

-- e.g., [3, 1, 2, 3, 2] -> 3
distance :: [Int] -> Int
distance [] = error "empty viewing distance"
distance (x:xs) | null t    = length h
                | otherwise = length h + 1
    where (h, t) = span (<x) xs

distances :: Grid Int -> Int -> Int -> [Int]
-- returns distances from [north, east, south, west] for g at i,j
distances g i j = map distance [north, east, south, west]
    where east  = drop j (row i g)
          west  = (reverse.take (j+1)) (row i g)
          north = (reverse.take (i+1)) (col j g)
          south = drop i (col j g)

score :: Grid Int -> Int -> Int -> Int
score g i j = product $ distances g i j

main :: IO ()
main = do
    input <- getContents
    let inputLines = lines input
    let grid :: Grid Int = map (map (read.return)) inputLines
    let (r, c) = (length grid, (length.head) grid)
    let cells = [ (i, j) | i <- [0..(r-1)], j <- [0..(c-1)]]
    print $ (maximum . map (uncurry (score grid))) cells