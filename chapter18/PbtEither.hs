data PbtEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PbtEither b) where
    fmap f (Left' a) = Left' (f a)
    fmap f (Right' b) = Right' b

instance Applicative (PbtEither b) where
    pure x = Left' x
    (Left' f) <*> (Left' a) = Left' (f a)
    (Left' f) <*> (Right' a) = Right' a
    (Right' f) <*> (Left' a) = Right' f
    (Right' f) <*> (Right' a) = Right' a

instance Monad (PbtEither b) where
    return = pure
    Left' a >>= f = f a
    Right' a >>= f = Right' a
