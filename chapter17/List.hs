data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil --[x] ---> x:[]
    --[f] <*> [x] = ?
    Nil <*> (Cons a x) = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> (Cons a as) = (fmap f (Cons a as)) `append` (fs <*> (Cons a as))
