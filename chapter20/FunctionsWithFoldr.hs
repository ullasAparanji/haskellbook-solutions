import Data.Foldable
import Data.Monoid

mySum :: (Foldable t, Num a) => t a -> a
mySum x = foldr (+) 0 x

myProduct :: (Foldable t, Num a) => t a -> a
myProduct x = foldr (*) 1 x

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem e x = getAny (foldMap (\a -> Any (a==e)) x)

myMinimum :: (Foldable t, Ord a, Num a) => t a ->  a
myMinimum x = foldr min 0 x

myNull :: (Foldable t) => t a -> Bool
myNull x = foldr (\a b -> False) True x

myLength :: (Foldable t) => t a -> Int
myLength x = getSum (foldMap (\a -> Sum 1) (x))

myToList :: (Foldable t) => t a -> [a]
myToList x = foldr (:) [] x

myFold :: (Foldable t, Monoid m) => t m -> m
myFold x = foldMap (id) x

myFoldMap :: (Foldable t, Monoid m, Functor t) => (a -> m) -> t a -> m
myFoldMap f x = foldr (<>) mempty (fmap f x)
