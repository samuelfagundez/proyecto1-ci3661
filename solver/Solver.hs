module Main where

import Solve
import System.Environment

main :: IO ()
main = do
  f <- getArgs
  if not (null f)
    then
      if (f !! 0) == "clever"
        then do
          putStrLn "Clever Wordle solver!"
          initialSolver Clever >>= (\i -> solveTheGame i)
        else do
          putStrLn "Naive Wordle solver!"
          initialSolver Naive >>= (\i -> solveTheGame i)
    else do
      putStrLn "Naive Wordle solver!"
      initialSolver Naive >>= (\i -> solveTheGame i)