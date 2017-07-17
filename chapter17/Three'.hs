import Data.Monoid

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a x y) = Three' a (f x) (f y)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a f g) <*> (Three' b x y) = Three' (a <> b) (f x) (g y)
