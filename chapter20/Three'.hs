data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f z (Three' a b c) = f b z
    foldMap f (Three' a b c) = f b
