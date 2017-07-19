filterF :: (Applicative f, Foldable f, Monoid (f a)) => 
           (a -> Bool) -> f a -> f a
filterF f x = foldMap (\a -> if (f a) then (pure a) else mempty) x
