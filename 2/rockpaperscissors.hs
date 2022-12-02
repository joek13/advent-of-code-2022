module Main where

import System.IO (getContents)
import Data.List (lines)
import Data.Bifunctor (bimap)

data Hand = Rock | Paper | Scissors
    deriving (Eq, Show)

data Outcome = Win | Lose | Draw
    deriving (Eq, Show)

mkHand :: String -> Hand
mkHand "A" = Rock
mkHand "B" = Paper
mkHand "C" = Scissors
mkHand "X" = Rock
mkHand "Y" = Paper
mkHand "Z" = Scissors

-- opponent's hand -> your hand -> outcome
getOutcome :: Hand -> Hand -> Outcome
getOutcome Rock Paper = Win
getOutcome Paper Scissors = Win
getOutcome Scissors Rock = Win
getOutcome x y
    | x == y = Draw
    | otherwise = Lose
    

handBonus :: Hand -> Int
handBonus Rock = 1
handBonus Paper = 2
handBonus Scissors = 3

outcomeBonus :: Outcome -> Int
outcomeBonus Lose = 0
outcomeBonus Draw = 3
outcomeBonus Win = 6

score :: Hand -> Hand -> Int
score x y = handBonus y + outcomeBonus (getOutcome x y)

main = do
    input <- getContents
    let inputLines = lines input -- read input as lines
    let inputLines' = map (span (/=' ')) inputLines -- split each line at the space
    let hands = map (bimap mkHand (mkHand.drop 1)) inputLines' -- convert each (String, String) to (Hand, Hand)
                                                               -- dropping the extra space at the beginning of snd
    print (sum (map (uncurry score) hands))