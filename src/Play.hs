module Play
where
import AA
import Match
import Util (loadDictionary, dictionary)
import System.Random

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

pickTarget :: AA.AA String String -> Target
pickTarget (Node _ _ value _ _) = T value
pickTarget _ = T ""