module Main where

--import Lib
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] | LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

main :: IO ()
main = do 
  args <- getArgs
  putStrLn . readExp $ args !! 0

readExp :: String -> String
readExp i = case parse parseExp "lispy" i of 
  Left err  -> "Oops :/ -> " ++ show err
  Right val -> "Found -> " ++ show val

symbol :: Parser Char
symbol = oneOf "g!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany space

parseAtom :: Parser LispVal
parseAtom = do 
  head <- letter <|> symbol
  tail <- many (letter <|> digit <|> symbol)
  let atom = [head] ++ tail
  pure $ case atom of
    "#t"      -> Bool True
    "#f"      -> Bool False
    otherwise -> Atom atom

--parseNumber :: Parser LispVal
--parseNumber = (Number . read) <$> (many1 digit)

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (\p -> read p)

parseString :: Parser LispVal
parseString = do
  char '"'
  string <- many1 (noneOf "\"")
  char '"'
  pure $ String string

parseExp :: Parser LispVal
parseExp = parseAtom 
       <|> parseString
       <|> parseNumber

