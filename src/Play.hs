{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Play
where
import AA
import Match
import Util (loadDictionary, dictionary)
import System.Random
import Data.Char

data GameState = GS { played :: Int
                    , won    :: Int
                    , streak :: Int
                    , target :: Target
                    , dict   :: AA.AA String String
}

data Result = Win Target | Lose Target

instance Show GameState where
  show (GS played won streak _ _) = show $ "Played: " ++ show played ++ " Won: " ++ show won ++ " Lost: " ++ show (played-won) ++ " Streak: " ++ show streak

instance Show Result where
  show (Win (T a)) = show "Got it! It was " ++ a ++ " \x1F60E "
  show (Lose (T a)) = show "Bummer! It was " ++ a ++ " \x1F480 "


initialState :: IO GameState
initialState = do
                dictionaryTree <- loadDictionary dictionary
                return $ GS 0 0 0 (T "") dictionaryTree

-- playTheGame :: IO GameState -> String
-- playTheGame gs = do
--                 show gs

-- play :: targetWord -> IO GameState
-- play target = do
--         putStr "Guess N? "
--         guessNumber <- getChar
--         guess <- getLine

-- fakeMain :: IO String
-- fakeMain = do
--             gameState <- initialState
--             return $ playTheGame gameState

pickTarget :: AA.AA String String -> IO Target
pickTarget Empty = return $ T ""
pickTarget t = do
                let treeToTarget = foldr (\x acc -> T x : acc) [] t
                rNum <- randomRIO (0, length treeToTarget - 1)
                return $ treeToTarget!!rNum 

-- It doesn't work because we need to handle the "backspace" and "enter" keystrokes operation
-- so I require a new function firm which is going to be:
-- readFive :: firstChar -> secondChar -> thirdChar -> fourthChar -> fifthChar -> sixthChar -> IO String
-- readFive a b c d e Enter = 
readFive :: IO String 
readFive = do
            content <- sequence [getChar,getChar,getChar,getChar,getChar]
            return $ [ toLower loweredString | loweredString <- content]