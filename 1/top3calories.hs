module Main where

import System.IO (getContents)
import Data.List (lines, sortOn)

splitList :: Eq a => a -> [a] -> [[a]]
splitList delim = splitList' delim []

splitList' :: Eq a => a -> [a] -> [a] -> [[a]]
splitList' delim [] [] = []
splitList' delim agg [] = [agg]
splitList' delim agg (x:xs) = if x == delim
                              then if (not.null) agg
                                then agg : splitList' delim [] xs
                                else splitList' delim [] xs
                              else splitList' delim (agg ++ [x]) xs

main = do 
    input <- getContents
    let inputLines = lines input
    let elves :: [[Int]] = map (map read) $ splitList "" inputLines
    print $ sum $ take 3 $ sortOn negate $ map sum elves