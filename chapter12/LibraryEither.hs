module LibraryEither where

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (Left a:xs) = a : lefts' xs
lefts' (Right a:xs) = lefts' xs

foo (Left a) [] = a:[]
foo (Left a) (x:xs) = a:x:xs
foo _ [] = []
foo _ (x:xs)= x:xs

left :: [Either a b] -> [a]
left (x:xs) = foldr (foo) [] (x:xs)

rights' :: [Either a b] -> [b]
rights' [] = []
rights' (Right b:xs) = b : rights' xs
rights' (Left a:xs) = rights' xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b
