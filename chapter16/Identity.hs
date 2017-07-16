import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g.f) x)

main :: IO ()
main = quickCheck (\x -> functorIdentity (x :: [Int]))

