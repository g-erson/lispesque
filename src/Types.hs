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

type ThrowsError = Either LispError

-- Nicer Show instance than deriving 
showL :: [LispVal] -> String
showL = unwords . map show 

instance Show LispError where 
  show (UnboundVar message varname)  = message <> ": " <> varname
  show (BadSpecialForm message form) = message <> ": " <> show form
  show (NotFunction message func)    = message <> ": " <> show func
  show (NumArgs expected found)      = "Expected " <> show expected 
                                            <> " args; found values " <> showL found
  show (TypeMismatch expected found) = "Invalid type: expected " <> expected
                                        <> ", found " <> show found
  show (Parser parseErr)             = "Parse error at " <> show parseErr


instance Show LispVal where
  show (String contents) = "\"" <> contents <> "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List l)          =  "(" <> showL l <> ")"
  show (DottedList l v)  = "(" <> showL l <> " . " <> show v <> ")"
