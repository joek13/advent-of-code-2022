{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Data.List (intercalate)

data Instr = Addx Int | Noop
  deriving (Show)

data MachineState = MachineState { regX :: Int }
  deriving (Show)

mkInstr :: String -> Instr
mkInstr str = case words str of
  ["addx", arg] -> Addx (read arg)
  ["noop"] -> Noop
  _ -> error ("unexpected instruction " ++ str)

-- given current machine state, returns list of machine states 
simulate :: MachineState -> Instr -> [MachineState]
simulate state Noop = [state]
simulate state (Addx x) = [state, state']
  where state' = MachineState { regX = (regX state) + x }

-- returns whether the ith pixel is on for a given state.
pixelOn :: Int -> MachineState -> Bool
pixelOn i s = (abs (col - (regX s))) <= 1
  where col = i `mod` 40

-- groups a list to chunks of size <= n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main :: IO ()
main = do
  -- parse input
  input <- getContents
  let inputLines = lines input
  let prog = map mkInstr inputLines
  let initialState = MachineState { regX = 1 }
  -- states !! i = machine state after ith cycle
  let states = foldl f [initialState] prog
               where f xs instr = xs ++ simulate (last xs) instr

  let pixels = zipWith pixelOn [0..] states

  putStrLn $ (intercalate "\n" . (map (map disp)) . chunksOf 40) pixels
    where disp True = '#'
          disp False = ' '