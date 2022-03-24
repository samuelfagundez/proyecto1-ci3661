module Solve where

import AA
import Match
import Util (dictionary, loadDictionary)

data Solver = Naive | Clever
  deriving (Eq)

data SolverState = GS
  { suggestion :: String,
    possible :: [String],
    remaining :: Int,
    dict :: AA.AA String String,
    strategy :: Solver
  }

initialState :: Solver -> IO SolverState
initialState x
  | x == Clever = do
    dictionarytree <- loadDictionary dictionary
    return $ GS "" [] (countTreeNodes dictionarytree) dictionarytree Clever
  | otherwise = do
    dictionarytree <- loadDictionary dictionary
    return $ GS "" [] (countTreeNodes dictionarytree) dictionarytree Naive

countTreeNodes :: AA k a -> Int
countTreeNodes Empty = 0
countTreeNodes (Node _ _ _ ltree rtree) = 1 + countTreeNodes ltree + countTreeNodes rtree

--solveTheGame:: SolverState->IO ()
--solveTheGame (GS )

sieve :: [Match] -> [String] -> [String]
sieve [] [] = []
sieve xs ys =
  let token = decompose [[], [], []] (zip xs [0 .. 4])
   in foldr (\s -> ((checkCorrect (token !! 2) $ checkMisplaced (token !! 1) $ checkAbsents (token !! 0) s) :)) [] ys
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

checkCorrect :: [(Char, Int)] -> String -> String
checkCorrect _ [] = []
checkCorrect [] palabra = palabra
checkCorrect xs palabra =
  if foldr (\(a, b) -> (a == (palabra !! b) &&)) True xs
    then palabra
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
