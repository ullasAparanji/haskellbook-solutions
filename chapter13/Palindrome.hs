import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (checkPal line1) of
        True -> putStrLn "It's a palindrome"
        False -> do
                   putStrLn "Nope!"
                   exitSuccess

checkPal :: String -> Bool
checkPal s = let word = filter isAlpha (map toLower s)
             in word == reverse word
