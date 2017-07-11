module LibraryMaybe where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing x = not (isJust x)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Nothing) = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just b) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a : xs) = a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = let g = catMaybes x in
              if length g == length x 
              then Just g
              else Nothing
