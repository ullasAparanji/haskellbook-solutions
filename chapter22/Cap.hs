import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap.rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled x = ((,) <$> cap <*> rev) x

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return (a,b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \a -> rev >>= \b -> return (a,b)
