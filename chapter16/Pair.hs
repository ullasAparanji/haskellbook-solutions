data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)
