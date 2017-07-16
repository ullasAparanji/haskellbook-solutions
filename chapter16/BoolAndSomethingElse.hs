import GHC.Arr

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish (f a)


