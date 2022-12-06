module Main where

import System.IO
import Data.List

maxCalories :: (Int, Int) -> String -> (Int, Int)
-- end of an inventory; reset cur and check if we have a new max
maxCalories (best, cur) "" = (max cur best, 0)
-- add current line's calories to running count
maxCalories (best, cur) next = (best, cur + read next)

main = do
    input <- getContents
    let inputLines = lines input
    print $ fst $ foldl maxCalories (0,0) inputLines