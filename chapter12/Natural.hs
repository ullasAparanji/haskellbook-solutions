module Natural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

go 0 = Zero
go n = Succ (go (n-1))

integerToNat :: Integer -> Maybe Nat
integerToNat n
             | n >= 0 = Just (go n)
             | otherwise = Nothing
