{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Match where

import Data.Char (toLower)
import Text.ParserCombinators.ReadPrec
import Text.Read

newtype Target = T [Char]

newtype Guess = G [Char]

data Match
  = Absent Char
  | Misplaced Char
  | Correct Char
  deriving (Eq)

instance Show Target where
  show (T a) = "It was " ++ show a

instance Show Guess where
  show (G a) = "Your guess " ++ show a

--El formato esta bien, más no consigo como hacer que los EMOJIS se muestren por consola, se muestra es su representación numerica
instance Show Match where
  show (Absent c) = show ("⬛" ++ [c])
  show (Misplaced c) = show ("🟨" ++ [c])
  show (Correct c) = show ("🟩" ++ [c])

--Como hacer que lea las representaciones de Match,
instance Read Match where
  readPrec =
    parens
      ( do
          g <- get
          if g == '⬛'
            then do
              f <- get
              if toLower f `elem` ['a' .. 'z']
                then pure (Absent (toLower f))
                else pfail
            else
              if g == '🟨'
                then do
                  f <- get
                  if toLower f `elem` ['a' .. 'z']
                    then pure (Misplaced (toLower f))
                    else pfail
                else
                  if g == '🟩'
                    then do
                      f <- get
                      if toLower f `elem` ['a' .. 'z']
                        then pure (Correct (toLower f))
                        else pfail
                    else pfail
      )

--Esta es una versión preliminar funcional, el detalle es que no ocurre como tal en 1 sola iteración,
--de momento esta es una versión de prueba hasta que se pueda mejorar
match :: Guess -> Target -> [Match]
match (G []) _ = []
match (G (x : xs)) (T (t : a : r : g : e : ws))
  | x == t = Correct x : match (G xs) (T (a : r : g : e : t : ws))
  | x == a = Misplaced x : match (G xs) (T (a : r : g : e : t : ws))
  | x == r = Misplaced x : match (G xs) (T (a : r : g : e : t : ws))
  | x == g = Misplaced x : match (G xs) (T (a : r : g : e : t : ws))
  | x == e = Misplaced x : match (G xs) (T (a : r : g : e : t : ws))
  | otherwise = Absent x : match (G xs) (T (a : r : g : e : t : ws))

--Función para saber si en una lista de [Match] son todos correct
fullmatch :: [Match] -> Bool
fullmatch [] = False
fullmatch [x] = case x of
  Absent w -> False
  Misplaced w -> False
  Correct w -> True
fullmatch (x : xs) = case x of
  Absent w -> False
  Misplaced w -> False
  Correct w -> fullmatch xs
