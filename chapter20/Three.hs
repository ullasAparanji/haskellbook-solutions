data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z
    foldMap f (Three a b c) = f c
