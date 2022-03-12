module Match where

data Target = Correcto | Incorrecto

data Guess = ArregloDeGuessings

data Match = Absent Char
            | Misplaced Char
            | Correct Char