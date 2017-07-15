import Data.Semigroup
import Test.QuickCheck

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = (BoolDisj True)
    _ <> (BoolDisj True) = (BoolDisj True)
    (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary :: Gen Bool
        return (BoolDisj x)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: BoolDisjAssoc)
