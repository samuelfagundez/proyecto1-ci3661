{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
module Play where 
import AA
import Match ( fullmatch, match, Guess(G), Target(..) )
import Util (loadDictionary, dictionary, turns, yesOrNo)
import System.Random ( Random(randomRIO) )
import Data.Char ( isLetter, toLower )
import System.IO ( hSetEcho, stdin, stdout, hFlush )
import GHC.IO.Handle (BufferMode(..), hSetBuffering)
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

pickTarget :: AA.AA String String -> IO Target
pickTarget Empty = return $ T ""
pickTarget t = do
                let treeToTarget = foldr (\x acc -> T x : acc) [] t
                rNum <- randomRIO (0, length treeToTarget - 1)
                return $ treeToTarget!!rNum

readFive :: IO [Char] -> IO [Char]
readFive s = do
                paramString <- s
                hSetEcho stdin False
                readChar <- getChar
                if length paramString == 5 && readChar /= '\n' && readChar /= '\DEL' && readChar /= '\BS'
                  then readFive s
                  else
                    if length paramString == 5 && readChar == '\n'
                      then
                        return $ pop $ auxToLower paramString ++ [readChar]
                      else
                        if (readChar == '\DEL' || readChar == '\BS') && not (null paramString)
                          then
                            do
                              putChar '\b'
                              putChar ' '
                              putChar '\b'
                              readFive $ return $ pop $ auxToLower paramString
                          else
                            if isLetter readChar
                              then 
                                do
                                  hFlush stdout
                                  putChar readChar
                                  hFlush stdout
                                  readFive $ return $ auxToLower $ paramString ++ [readChar]
                              else readFive s
                    where auxToLower s = [ toLower loweredString | loweredString <- s]

playTheGame :: IO GameState -> IO ()
playTheGame gs = do
  (GS played won streak target dict) <- gs
  t <- pickTarget dict
  playedGs <- play turns (return (GS played won streak t dict))
  putStrLn ""
  print playedGs
  playAgain <- yesOrNo "Play again"
  if playAgain
    then
      do
        putStrLn ""
        playTheGame $ return playedGs
    else putStrLn ""

play :: Int -> IO GameState -> IO GameState
play 0 gs = do
  (GS played won streak target dict) <- gs
  return (GS (played+1) won 0 target dict)
play remainingTurns gs = do
    (GS played won streak target dict) <- gs
    if remainingTurns < turns
      then putStrLn ""
      else putStr ""
    hSetBuffering stdout NoBuffering
    putStr $ "Guess " ++ show (turns - remainingTurns + 1) ++ "? "
    hSetEcho stdout True
    guessWord <- readFive $ return ""
    let gameResult = match (G guessWord) target
    if remainingTurns == 1
      then
        putStr $ " Your guess '" ++ guessWord ++ "' is not a valid word!"
      else
        putStr $ " " ++ show gameResult
    if fullmatch gameResult
      then return (GS (played+1) (won+1) (streak+1) target dict)
      else
        do
          play (remainingTurns-1) gs