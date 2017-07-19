import Data.Monoid
import Data.Foldable

data Constant a b = Constant a deriving Show

instance Monoid a => Foldable (Constant a) where
    --foldr f z (Constant a) = f mempty z
    foldMap f (Constant a) = f a
