module Util where

import Data.Char ( toLower )
import AA ( AA, empty, insert )
import GHC.IO.Handle (hSetEcho)
import System.IO ( hSetEcho, stdin, stdout )

turns :: Int
turns = 3

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
    putStr $ s ++ " (y/n)? "
    decision <- getChar
    if decision == 'y' || decision == 'n'
        then return $ decision == 'y'
        else yesOrNo s