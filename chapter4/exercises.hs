-- isPalindrome.hs
module Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (list == reverse list)

isPalindromeStr :: [Char] -> Bool
isPalindromeStr str = (stripped == reverse stripped)
  where stripped = filter (/=' ') str

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
    then x
  else
    (-x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
