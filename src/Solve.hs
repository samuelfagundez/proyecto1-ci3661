{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solve
  ( Solver (..),
    initialSolver,
    solveTheGame,
    sieve,
    cleverpick,
  )
where

import AA
import Match
import System.Random
import Text.Read (Lexeme (Char, String), readMaybe)
import Util (dictionary, loadDictionary, turns, yesOrNo)

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
solveTheGame (GS a xs r tree stra) =
  do
    solveTheGameTurnos (GS a xs r tree stra) 1
    c <- yesOrNo "Solve Another"
    if c
      then do
        f <- solveTheGame (GS "" (foldr (\k -> ([k] ++)) [] tree) (foldr (\k -> (1 +)) 0 tree) tree stra)
        putStr ""
      else putStr ""

--solveTheGameTurnos :: Monad m => SolverState -> p -> m SolverState
solveTheGameTurnos (GS a xs r tree stra) n
  | n <= turns && r == 1 =
    do
      print (GS a xs r tree stra)
      return (GS a xs r tree stra)
  | n > 0 && n <= turns = do
    print (GS a xs r tree stra)
    m <- reading n
    if stra == Naive
      then do
        j <- naive m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
      else do
        let j = clever m (GS a xs r tree stra)
        solveTheGameTurnos j (n + 1)
  | n == (turns + 1) = do
    print (GS a xs r tree stra)
    putStrLn "You lost! 🤭"
    return (GS a xs r tree stra)

reading n
  | n > 0 && n < (turns -1) = do
    putStr ("Hint " ++ show n ++ " 🤔? ")
    l <- getLine
    let m = readMaybe l :: Maybe [Match]
    case m of
      Just y -> return y
      Nothing -> reading n
  | n == (turns -1) = do
    putStr ("Hint " ++ show n ++ " 🙁? ")
    l <- getLine
    let m = readMaybe l :: Maybe [Match]
    case m of
      Just y -> return y
      Nothing -> reading n
  | n == turns = do
    putStr ("Hint " ++ show n ++ " 😬? ")
    l <- getLine
    let m = readMaybe l :: Maybe [Match]
    case m of
      Just y -> return y
      Nothing -> reading n

sieve :: [Match] -> [String] -> [String]
sieve [] [] = []
sieve xs ys =
  let token = decompose [[], [], []] (zip xs [0 .. 4])
   in foldr (\s -> ((checkCorrect (token !! 2) $ checkMisplaced (token !! 1) $ checkAbsents (token !! 0) s) ++)) [] ys

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
  if foldr (\(a, c) -> ((a `elem` palabra && (a /= palabra !! c)) ||)) False xs
    then palabra
    else []

checkCorrect :: [(Char, Int)] -> String -> [String]
checkCorrect _ [] = []
checkCorrect [] palabra = [palabra]
checkCorrect xs palabra =
  if foldr (\(a, b) -> (a == (palabra !! b) &&)) True xs
    then [palabra]
    else []

naive :: [Match] -> SolverState -> IO SolverState
naive xs (GS sugg poss rem dict stra) =
  do
    let posibles = sieve xs poss
    let size = length posibles
    r <- randomRIO (0, size -1)
    return $ GS (posibles !! r) posibles size dict stra

--Donde Va Hola tiene que ir un selector al Azar y se coloca posibles!!(selector al azar entre 0 y lentgh posibles)

clever :: [Match] -> SolverState -> SolverState
clever xs (GS sugg poss rem dict stra) =
  let posibles = sieve xs poss
   in GS (cleverpick posibles) posibles (length posibles) dict stra

cleverpick :: [String] -> String
cleverpick = foldr compareS ""
  where
    compareS w z
      | w == "" = z
      | z == "" = w
      | otherwise =
        if points w 0 >= points z 0
          then w
          else z
      where
        points :: [Char] -> Int -> Int
        points [] n = n
        points (l : ls) n =
          if l `elem` ls
            then points ls n
            else points ls (n + 1)
