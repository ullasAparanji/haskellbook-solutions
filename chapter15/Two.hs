import Data.Semigroup
import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
    x <> _ = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoAssoc = Two Int String -> Two Int String -> Two Int String -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: TwoAssoc)
