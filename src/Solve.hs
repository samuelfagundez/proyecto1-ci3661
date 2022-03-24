{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solve
  ( Solver (..),
    initialSolver,
    solveTheGame,
  )
where

import AA
import Match
import Util (dictionary, loadDictionary, turns)

data Solver = Naive | Clever
  deriving (Eq)

data SolverState = GS
  { suggestion :: String,
    possible :: [String],
    remaining :: Int,
    dict :: AA.AA String String,
    strategy :: Solver
  }

instance Show SolverState where
  show (GS [] xs r _ _) = "There are " ++ show r ++ " possible words."
  show (GS _ [x] 1 _ _) = "It must be " ++ "«" ++ x ++ "»"
  show (GS s xs r _ _) = show r ++ " words remain. I suggest «" ++ s ++ "»."

initialSolver :: Solver -> IO SolverState
initialSolver x
  | x == Clever = do
    dictionarytree <- loadDictionary dictionary
    return $ GS [] (foldr (\k -> ([k] ++)) [] dictionarytree) (foldr (\k -> (1 +)) 0 dictionarytree) dictionarytree Clever
  | otherwise = do
    dictionarytree <- loadDictionary dictionary
    return $ GS [] (foldr (\k -> ([k] ++)) [] dictionarytree) (foldr (\k -> (1 +)) 0 dictionarytree) dictionarytree Naive

--solveTheGame::
solveTheGame x =
  do
    solveTheGameTurnos x 1
    putStr "Solve another (y/n)? "
    c <- getChar
    if c == 'y'
      then solveTheGameTurnos x 1
      else return x

solveTheGameTurnos (GS "" xs r tree stra) n =
  do
    print (GS "" xs r tree stra)
    putStr ("Hint " ++ show n ++ " 🤔?")
    l <- getLine
    let m = read l :: [Match]
    if stra == Naive
      then do
        let j = naive m (GS "" xs r tree stra)
        solveTheGameTurnos j (n + 1)
      else do
        let j = clever m (GS "" xs r tree stra)
        solveTheGameTurnos j (n + 1)
solveTheGameTurnos (GS a xs r tree stra) n
  | n <= turns && r == 1 =
    do
      print (GS a xs r tree stra)
      return (GS a xs r tree stra)
  | n > 1 && n < (turns -1) = do
    print (GS a xs r tree stra)
    putStr ("Hint " ++ show n ++ " 🤔?")
    l <- getLine
    let m = read l :: [Match]
    if stra == Naive
      then do
        let j = naive m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
      else do
        let j = clever m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
  | n == (turns -1) = do
    print (GS a xs r tree stra)
    putStr ("Hint " ++ show n ++ " 🙁?")
    l <- getLine
    let m = read l :: [Match]
    if stra == Naive
      then do
        let j = naive m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
      else do
        let j = clever m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
  | n == turns = do
    print (GS a xs r tree stra)
    putStr ("Hint " ++ show n ++ " 😬?")
    l <- getLine
    let m = read l :: [Match]
    if stra == Naive
      then do
        let j = naive m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
      else do
        let j = clever m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
  | n == (turns + 1) = do
    print (GS a xs r tree stra)
    putStrLn "You lost! 🤭"
    return (GS a xs r tree stra)

sieve :: [Match] -> [String] -> [String]
sieve [] [] = []
sieve xs ys =
  let token = decompose [[], [], []] (zip xs [0 .. 4])
   in foldr (\s -> ((checkCorrect (token !! 2) $ checkMisplaced (token !! 1) $ checkAbsents (token !! 0) s) ++)) [] ys
  where
    decompose rs [] = rs
    decompose rs ((z, a) : zs) = case z of
      Absent w -> decompose [(rs !! 0) ++ [(w, a)], rs !! 1, rs !! 2] zs
      Misplaced w -> decompose [rs !! 0, rs !! 1 ++ [(w, a)], rs !! 2] zs
      Correct w -> decompose [rs !! 0, rs !! 1, rs !! 2 ++ [(w, a)]] zs

checkAbsents :: [(Char, Int)] -> String -> String
checkAbsents _ [] = []
checkAbsents [] palabra = palabra
checkAbsents xs palabra =
  if foldr (\(a, _) -> (a `elem` palabra ||)) False xs
    then []
    else palabra

checkMisplaced :: [(Char, Int)] -> String -> String
checkMisplaced _ [] = []
checkMisplaced [] palabra = palabra
checkMisplaced xs palabra =
  if foldr (\(a, _) -> (a `elem` palabra ||)) False xs
    then palabra
    else []

checkCorrect :: [(Char, Int)] -> String -> [String]
checkCorrect _ [] = []
checkCorrect [] palabra = [palabra]
checkCorrect xs palabra =
  if foldr (\(a, b) -> (a == (palabra !! b) &&)) True xs
    then [palabra]
    else []

naive :: [Match] -> SolverState -> SolverState
naive xs (GS sugg poss rem dict stra) =
  let posibles = sieve xs poss
   in GS "hola" posibles (length posibles) dict stra

--Donde Va Hola tiene que ir un selector al Azar y se coloca posibles!!(selector al azar entre 0 y lentgh posibles)

clever :: [Match] -> SolverState -> SolverState
clever xs (GS sugg poss rem dict stra) =
  let posibles = sieve xs poss
   in GS "hola" posibles (length posibles) dict stra
