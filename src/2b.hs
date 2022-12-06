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

mkOutcome :: String -> Outcome
mkOutcome "X" = Lose
mkOutcome "Y" = Draw
mkOutcome "Z" = Win

beats :: Hand -> Hand
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

loses :: Hand -> Hand
loses Rock = Scissors
loses Paper = Rock
loses Scissors = Paper

selectHand :: Hand -> Outcome -> Hand
selectHand x Draw = x
selectHand x Win = beats x
selectHand x Lose = loses x

handBonus :: Hand -> Int
handBonus Rock = 1
handBonus Paper = 2
handBonus Scissors = 3

outcomeBonus :: Outcome -> Int
outcomeBonus Lose = 0
outcomeBonus Draw = 3
outcomeBonus Win = 6

score :: Hand -> Outcome -> Int
score hand outcome = handBonus hand + outcomeBonus outcome

main = do
    input <- getContents
    let inputLines = lines input -- read input as lines
    let inputLines' = map (span (/=' ')) inputLines -- split each line at the space
    let pairs = map (bimap mkHand (mkOutcome.drop 1)) inputLines' -- (opponent hand, outcome)
    let hands = map (uncurry selectHand) pairs -- my hand
    let scoredPairs = zip hands (map snd pairs) -- (my hand, outcome)
    print (sum (map (uncurry score) scoredPairs))