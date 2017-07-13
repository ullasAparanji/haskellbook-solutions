import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    a <- getLine
    let age = read a::Integer
    let p = mkPerson name age
    if check p
    then putStrLn $ "Yay! Successfully got a person!" ++ show p
    else putStrLn "Error"
    where check (Left _) = False
          check (Right _) = True

