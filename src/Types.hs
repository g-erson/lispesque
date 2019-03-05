module Types where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Nicer Show instance than deriving 
--
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
