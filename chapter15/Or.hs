import Control.Monad (liftM)
import Data.Semigroup
import Test.QuickCheck

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst a) <> (Fst b) = (Fst b)
    (Fst a) <> (Snd b) = (Snd b)
    (Snd a) <> (Fst b) = (Snd a)
    (Snd a) <> (Snd b) = (Snd b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = oneof [liftM Fst arbitrary, liftM Snd arbitrary]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: OrAssoc)
