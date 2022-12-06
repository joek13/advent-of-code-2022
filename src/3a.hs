module Main where

import System.IO (getContents)
import Data.List (lines)
import Data.Char (ord)
import qualified Data.Set as Set (fromList, intersection, map)

-- convert char to its priority
priority :: Char -> Int
priority c | ord c >= ord 'a' = ord c - ord 'a' + 1
           | otherwise        = ord c - ord 'A' + 27

solve :: String -> String -> Int
solve as bs = sum $ Set.map priority common
    where as' = Set.fromList as
          bs' = Set.fromList bs
          common = Set.intersection as' bs'

splitLine :: [a] -> ([a], [a])
splitLine xs = splitAt l xs
    where l = length xs `div` 2

main = do
    input <- getContents
    let inputLines = lines input
    let pairs = map splitLine inputLines
    print $ sum $ map (uncurry solve) pairs