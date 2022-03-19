module Util where

import Data.Char
import System.IO
import AA
import Control.Monad
clear = putStr "\ESC[2J"

turns :: Int
turns = 6

dictionary :: FilePath
dictionary = "american-english"

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter (\x -> take 2 (reverse x) /= "s'" && length x == 5)

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary fp = do
        handle <- openFile fp ReadMode
        contents <- hGetContents handle
        -- This line works as a waiter for the lazy hGetContents
        putStrLn contents
        clear
        let singlewords = fiveLetterWords $ words contents
        let wordsTree = foldr (\word tree -> AA.insert word word tree) AA.empty singlewords
        hClose handle
        return wordsTree

yesOrNo :: String -> IO Bool
yesOrNo s = do
    putStrLn $ s ++ " (y/n)?"
    decision <- getChar
    if decision == 'y' || decision == 'n'
        then return $ decision == 'y'
        else yesOrNo s