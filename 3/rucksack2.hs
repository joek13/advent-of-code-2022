module Main where

import System.IO (getContents)
import Data.List (lines)
import Data.Char (ord)
import qualified Data.Set as Set (fromList, intersection, map, empty)

-- convert char to its priority
priority :: Char -> Int
priority c | ord c >= ord 'a' = ord c - ord 'a' + 1
           | otherwise        = ord c - ord 'A' + 27

-- groups a list to chunks of size <= n
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

solve :: [String] -> Int
solve xs = sum $ Set.map priority common
    where sets = map Set.fromList xs
          common = foldr1 Set.intersection sets

main = do
    input <- getContents
    let inputLines = lines input
    let groups = chunksOf 3 inputLines
    print $ sum $ map solve groups