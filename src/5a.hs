module Main where

import Text.Regex.TDFA ( (=~) )

data Instr = Move Int Int Int
    deriving Show

-- groups a list to chunks of size <= n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- regex for "move count from src to dest" instructions
instrPattern :: String
instrPattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)"

-- parses "move x from y to z" into (Move x y z)
mkInstr :: String -> Instr
mkInstr str = Move (read count) (read src) (read dest)
    where getVals [[_, a, b, c]] = (a, b, c)
          getVals _ = error "failed to parse instruction"
          (count, src, dest)= getVals (str =~ instrPattern)

-- given current stacks and new line, parses line and adds to stacks
mkStacks :: [String] -> String -> [String]
mkStacks stacks line = zipWith push letters stacks
    where chunks  = chunksOf 4 line
          -- skip [ in [A]
          letters = map (!! 1) chunks
          -- do not add spaces to stack
          push ' ' xs = xs
          push ch xs = ch:xs


-- reads lines in input and constructs stacks
readStacks :: IO [String]
readStacks = do
    line <- getLine
    if '[' `elem` line
        then do
            recur <- readStacks
            return (mkStacks recur line)
        else return (repeat "")

-- given an instruction, executes it and returns new stacks
execInstr :: [String] -> Instr -> [String]
execInstr stacks (Move 0 _ _) = stacks
execInstr stacks (Move count src dest) = execInstr stacks' (Move (count-1) src dest)
    where target = head (stacks !! (src-1))
          stacks' = zipWith update [1..] stacks
          update i xs | i == src = drop 1 xs
                      | i == dest = target : xs
                      | otherwise = xs
                

main :: IO ()
main = do
    stacks <- readStacks -- read in stacks
    _ <- getLine -- skip a line
    inputLines <- lines <$> getContents -- get remaining lines
    let instrs = map mkInstr inputLines -- parse instructions
    let finalStacks = foldl execInstr stacks instrs -- execute instructions
    print $ map head finalStacks