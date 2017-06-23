type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
  | AgeTooLow
  | PersonInvalidUnkown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnkown $
    "Name was: " ++ show name ++
    " Age was: " ++ show age

handlePerson :: (Either PersonInvalid Person) -> String
handlePerson (Left p) = show p
handlePerson (Right p) = "Yay! Successfuly got a person: " ++ show p

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Name: "
  name <- getLine
  putStrLn "Age: "
  age <- getLine
  putStrLn $ handlePerson $ mkPerson name (read age)
  return ()
