module JohnDoe where

data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Show)

fullName :: Person -> String
fullName person = firstName person ++ " " ++ lastName person

greet :: Person -> String
greet person = "Hello, " ++ fullName person ++ "!"
