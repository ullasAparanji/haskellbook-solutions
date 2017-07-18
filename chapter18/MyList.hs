import Control.Monad (join)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> Nil = Nil
    Nil <*> (Cons a b) = Nil
    (Cons a b) <*> Nil = Nil
    (Cons f g) <*> (Cons a b) = Cons (f a) (g <*> b)

instance Monad List where
    return = pure
    Nil >>= f = Nil
    (Cons a b) >>= f = join (Cons (f a) (fmap f b))
