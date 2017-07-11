module Anamorphism where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = getFirst (f b) : myUnfoldr f (getSecond (f b))
                where getFirst (Just (a,b)) = a
                      getSecond :: Maybe (a,b) -> b
                      getSecond (Just (a,b)) = b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\b -> Just (b, (f b))) a
