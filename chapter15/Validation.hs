import Control.Monad (liftM)
import Data.Semigroup
import Test.QuickCheck

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup (Validation a b) where
    (Failure' a) <> _ = (Failure' a)
    _ <> (Failure' a) = (Failure' a)
    (Success' a) <> (Success' b) = (Success' a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = oneof [liftM Failure' arbitrary, liftM Success' arbitrary]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ValidationAssoc = Validation Int String -> Validation Int String -> Validation Int String -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: ValidationAssoc)
