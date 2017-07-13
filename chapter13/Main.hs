module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
              dict <- readFile "data/dict.txt"
              return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
               aw <- allWords
               return (filter gameLength aw)
               where
                     gameLength w = let l = length w
                                    in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
                   randomIndex <- randomRIO (0, (length wl) - 1)
                   return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>=  randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] [Char]
instance Show Puzzle where
         show (Puzzle _ discovered guessed _) = (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (\x -> Nothing) word) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) x = elem x word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) x = elem x guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s i) c =
    Puzzle word newFilledInSoFar (c : s) newInCorrect
    where
         zipper guessed wordChar guessChar = if wordChar == guessed
                                             then Just wordChar
                                             else guessChar
         newFilledInSoFar = zipWith (zipper c) word filledInSoFar
         newInCorrect = if elem c word
                        then i
                        else (c : i)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
                        putStrLn "You already guessed that character. Pick something else!"
                        return puzzle
        (True, _) -> do
                        putStrLn "This character was in word. Filling up..."
                        return (fillInCharacter puzzle guess)
        (False, _) -> do
                        putStrLn "This character wasn't in word. Try again..."
                        return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed incorrect) = if (length incorrect) > 7 
                                          then do
                                                  putStrLn "You lose!"
                                                  putStrLn $ "The word was: " ++ wordToGuess
                                                  exitSuccess
                                          else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = if all isJust filledInSoFar
                                     then do
                                             putStrLn "You win!"
                                             exitSuccess
                                     else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
          hSetBuffering stdout NoBuffering
          word <- randomWord'
          let puzzle = freshPuzzle (fmap toLower word)
          runGame puzzle
