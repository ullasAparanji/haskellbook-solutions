import Data.Semigroup
import Test.QuickCheck

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj False) <> _ = (BoolConj False)
    _ <> (BoolConj False) = (BoolConj False)
    (BoolConj True) <> (BoolConj True) = (BoolConj True)

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary :: Gen Bool
        return (BoolConj x)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: BoolConjAssoc)
