import Data.Semigroup
import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
    x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
                   x <- arbitrary
                   return (Identity x)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc = Identity Int -> Identity Int -> Identity Int -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: IdentityAssoc)
