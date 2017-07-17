data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure x = Second x
    (First a) <*> (First b) = First a
    (First a) <*> (Second b) = First a
    (Second f) <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)
