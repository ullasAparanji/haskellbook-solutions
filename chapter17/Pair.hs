data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair a b) <*> (Pair c d) = Pair (a c) (b d)
