import Data.Monoid

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Error e) = Error e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure x = Success x
    (Error f) <*> (Error a) = Error (f <> a)
    (Error f) <*> (Success a) = Error f
    (Success f) <*> (Error g) = Error g
    (Success f) <*> (Success g) = Success (f g)
