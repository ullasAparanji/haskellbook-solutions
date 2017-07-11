module StringProcessing where

import Data.Char

vowels = "aeiouAEIOU"

notThe :: String -> Maybe String
notThe x
       | x == "the" = Nothing
       | otherwise = Just x

replaceThe :: String -> String
replaceThe x = unwords (go (words x))
               where go [] = []
                     go (w:ws)
                         | notThe w == Nothing = "a" : go ws
                         | otherwise = w : go ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = go (words x)
                       where
                        go [] = 0
                        go (w:w2:ws)
                           | (notThe w == Nothing) && (beginsWithVowel w2) = 1 + go ws
                           | otherwise = go (w2:ws)
                        go _ = 0
                        beginsWithVowel s = s!!0 `elem` vowels


countVowels :: String -> Integer
countVowels "" = 0
countVowels (w:ws)
            | w `elem` vowels = 1 + countVowels ws
            | otherwise = countVowels ws

countConsonants :: String -> Integer
countConsonants "" = 0
countConsonants (w:ws)
                | isAlpha w && not (elem w vowels) = 1 + countConsonants ws
                | otherwise = countConsonants ws

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if countVowels s > countConsonants s
           then Nothing
           else Just (Word' s)
