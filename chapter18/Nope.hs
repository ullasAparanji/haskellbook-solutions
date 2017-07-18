data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure x = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg
