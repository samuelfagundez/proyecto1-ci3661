{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Play
where
import AA
import Match
import Util (loadDictionary, dictionary)
import System.Random
import Data.Char
clear = putStr "\ESC[2J"
data GameState = GS { played :: Int
                    , won    :: Int
                    , streak :: Int
                    , target :: Target
                    , dict   :: AA.AA String String
}

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

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

readFive :: IO [Char] -> IO [Char]
readFive s = do
                putStrLn "\ESC[2J"
                paramString <- s
                putStrLn paramString
                readChar <- getChar
                let newString = paramString ++ [readChar]
                if length paramString == 5 && readChar == '\n'
                  then
                    return $ pop $ auxToLower newString
                  else
                    if (readChar == '\DEL' || readChar == '\BS') && not (null paramString)
                      then
                        readFive $ return $ pop $ auxToLower paramString
                      else
                        if isLetter readChar
                          then readFive $ return $ auxToLower newString
                          else readFive s
                where auxToLower s = [ toLower loweredString | loweredString <- s]