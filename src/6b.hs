module Main where

import Data.List (tails, find)
import Data.Set (Set, empty, member, insert)
import Data.Maybe (fromJust)

uniq :: Ord a => [a] -> Bool
uniq = uniq' empty 

uniq' :: Ord a => Set a -> [a] -> Bool
uniq' seen (x:xs) | x `member` seen = False
                  | otherwise = uniq' (insert x seen) xs
uniq' _ [] = True

main :: IO ()
main = do
    input <- getLine
    let seqs = map (take 14) $ tails input
    let pairs  = zip [0..] seqs :: [(Int, [Char])]
    let marker = fst $ fromJust $ find (uniq.snd) pairs
    print $ 14 + marker