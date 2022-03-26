module Util where

import Data.Char ( toLower )
import AA ( AA, empty, insert )
import System.IO ( hSetEcho, stdin, stdout )
import GHC.IO.Handle (BufferMode(..), hSetBuffering)

turns :: Int
turns = 6

dictionary :: FilePath
dictionary = "american-english"

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter (\x -> take 2 (reverse x) /= "s'" && length x == 5)

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary path = do contents <- readFile path
                         let singlewords = fiveLetterWords $ lines contents
                         let wordsTree = foldr (\word tree -> AA.insert word [ toLower loweredString | loweredString <- word] tree) AA.empty singlewords
                         return wordsTree

yesOrNo :: String -> IO Bool
yesOrNo s = do
    hSetEcho stdout False
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    putStr $ s ++ " (y/n)? "
    decision <- getChar
    if decision == 'y' || decision == 'n'
        then return $ decision == 'y'
        else yesOrNo s