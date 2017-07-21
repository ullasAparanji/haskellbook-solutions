newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (f.ra)

instance Applicative (Reader r) where
    pure x = Reader (\a -> x)
    (Reader ra) <*> (Reader rb) = Reader (ra <*> rb)
