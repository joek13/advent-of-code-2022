{-# LANGUAGE NamedFieldPuns #-}

module Main where

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

  -- states !! 19 = state before/during 20th cycle
  let indices = map (+ (-1)) [20, 60, 100, 140, 180, 220]
  let strength = map (\i -> (regX (states !! i)) * (i+1)) indices
    
  print $ sum $ strength