import Data.Monoid

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (Two a b) <*> (Two c d) = Two (a <> c) (b d)
