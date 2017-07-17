import Data.Monoid

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four a b c d) <*> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d d')
