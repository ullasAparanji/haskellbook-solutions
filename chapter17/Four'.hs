import Data.Monoid

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    (Four' a b c d) <*> (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (d d')
