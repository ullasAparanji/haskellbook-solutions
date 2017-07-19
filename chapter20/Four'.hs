data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
    foldr f z (Four' a b c d) = f b z
    foldMap f (Four' a b c d) = f b
