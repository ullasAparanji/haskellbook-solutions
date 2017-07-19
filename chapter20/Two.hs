data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two a b) = f b z
    foldMap f (Two a b) = f b
