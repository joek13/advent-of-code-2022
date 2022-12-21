module Main where

type Grid a = [[a]]

col :: Int -> Grid a -> [a]
-- gets the jth column of a grid
col j = map (!! j)

row :: Int -> Grid a -> [a]
-- gets the ith row of a grid
row i g = g !! i

visible :: Ord a => Int -> Int -> Grid a -> Bool
visible i j g = 
    let val = (g !! i) !! j in
    -- visible from west
    all (<val) (take j (row i g))
    -- visible from east
    || all (<val) (drop (j+1) (row i g))
    -- visible from north
    || all (<val) (take i (col j g))
    -- visible from south
    || all (<val) (drop (i+1) (col j g))

count :: (a -> Bool) -> [a] -> Int
count f = foldl f' 0
    where f' agg x | f x = agg + 1
                   | otherwise = agg

main :: IO ()
main = do
    input <- getContents
    let inputLines = lines input
    let grid :: Grid Int = map (map (read.return)) inputLines
    let (r, c) = (length grid, (length.head) grid)
    let cells = [ (i, j) | i <- [0..(r-1)], j <- [0..(c-1)]]
    print $ count id $ map (\(i,j) -> visible i j grid) cells