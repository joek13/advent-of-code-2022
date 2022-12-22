module Main where

import Control.Monad.Writer (Writer, tell, foldM, execWriter)
import Data.Set (fromList)

type Move = (Int, Int)
type Pos = (Int, Int)

mkMove :: String -> [Move]
mkMove str = case words str of
        [dir, amt] -> replicate (read amt) (parseDir dir)
        _ -> error ("invalid move " ++ str)
    where parseDir "U" = (-1, 0)
          parseDir "R" = (0, 1)
          parseDir "D" = (1, 0)
          parseDir "L" = (0, -1)
          parseDir d = error ("unerpected direction " ++ d)

dist :: Pos -> Pos -> Int
-- l-infty norm
dist (r1, c1) (r2, c2) = max (abs (r1 - r2)) (abs (c1 - c2))

nextTailPos :: Pos -> Pos -> Pos
-- (tail pos, head pos) -> new tail pos
nextTailPos (rTail, cTail) (rHead, cHead)
    | dist (rTail, cTail) (rHead, cHead) <= 1 = (rTail, cTail)
    | otherwise = (rTail + dr, cTail + dc)
    where dr = signum (rHead - rTail)
          dc = signum (cHead - cTail)

update :: Pos -> Pos -> Move -> (Pos, Pos)
update tailPos (headR, headC) (dr, dc) =
    let headPos' = (headR + dr, headC + dc)
        tailPos' = nextTailPos tailPos headPos'
    in (tailPos', headPos')

step :: (Pos, Pos) -> Move -> Writer [Pos] (Pos, Pos)
step (tailPos,headPos) move = do
    let (tailPos', headPos') = update tailPos headPos move
    tell [tailPos']
    return (tailPos', headPos')

main :: IO ()
main = do
    input <- getContents
    let moves = concatMap mkMove (lines input)
    let tailPositions = execWriter $ foldM step ((0,0),(0,0)) moves
    print $ length $ fromList tailPositions