module Main where

import System.IO (getContents)
import Data.List (lines)
import Data.Bifunctor (bimap)

type Range = (Int, Int)

-- splits list on first occurence of a given element
splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn delim xs = (as, bs)
    where
        (as, bs') = span (/= delim) xs -- split on separator
        bs = drop 1 bs'                -- drop separator


-- makes a range from string like 1-10
mkRange :: String -> Range
mkRange str = bimap read read (splitOn '-' str)

-- returns if ranges overlap
overlap :: Range -> Range -> Bool
overlap a b =    (b1 <= a1 && a1 <= b2) || (b1 <= a2 && a2 <= b2)
              || (a1 <= b1 && b1 <= a2) || (a1 <= b2 && b2 <= a2)
    where (a1, a2) = a
          (b1, b2) = b

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
    input <- getContents
    let inputLines = lines input
    let ranges = map (bimap mkRange mkRange.splitOn ',') inputLines
    print (count (uncurry overlap) ranges)