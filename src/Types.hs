module Types where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char

showL :: [LispVal] -> String
showL = unwords . map show 

instance Show LispVal where
  show (String contents) = "\"" <> contents <> "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List l)          =  "(" <> showL l <> ")"
  show (DottedList l v)  = "(" <> showL l <> " . " <> show v <> ")"
