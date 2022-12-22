module Main where

import Control.Monad.Writer (Writer, execWriter, foldM, tell)
import Data.Set (fromList)

type Pos = (Int, Int)

type Move = (Int, Int)

type Rope = [Pos]

mkMove :: String -> [Move]
mkMove str = case words str of
  [dir, amt] -> replicate (read amt) (parseDir dir)
  _ -> error ("invalid move " ++ str)
  where
    parseDir "U" = (-1, 0)
    parseDir "R" = (0, 1)
    parseDir "D" = (1, 0)
    parseDir "L" = (0, -1)
    parseDir d = error ("unexpected direction " ++ d)

move :: Pos -> Move -> Pos
move (r, c) (dr, dc) = (r + dr, c + dc)

-- l-infty norm of two ordered pairs
dist :: Pos -> Pos -> Int
dist (r1, c1) (r2, c2) = max (abs (r1 - r2)) (abs (c1 - c2))

-- for two-segment rope:
-- given head position and tail position, computes direction tail segment should move
-- head pos -> tail pos -> move for tail
nextMove :: Pos -> Pos -> Move
nextMove (rHead, cHead) (rTail, cTail)
  -- if the tail and head are adjacent, do nothing
  | dist (rTail, cTail) (rHead, cHead) <= 1 = (0, 0)
  -- otherwise, move the tail one unit towards head on each axis
  | otherwise = (dr, dc)
  where
    dr = signum (rHead - rTail)
    dc = signum (cHead - cTail)

-- moves the head and updates the rope
update :: Rope -> Move -> Rope
update (h : t : xs) m = h' : update (t : xs) m'
  where
    h' = move h m
    m' = nextMove h' t
update [h] m = [move h m]
update [] _ = []

-- given rope and next move, performs the move
-- writes the position of the end of the rope to the writer
step :: Rope -> Move -> Writer [Pos] Rope
step rope m = do
  let rope' = update rope m
  tell [last rope']
  return rope'

main :: IO ()
main = do
  input <- getContents
  let moves = concatMap mkMove (lines input)
  let initialRope = replicate 10 (0, 0) :: [(Int, Int)]
  let tailPositions = execWriter $ foldM step initialRope moves
  print $ length $ fromList tailPositions