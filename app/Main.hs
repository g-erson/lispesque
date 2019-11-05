module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Evaluator
import Types

--Main Parser

main :: IO ()
main = getArgs >>= print . eval . readExp . head

readExp :: String -> LispVal
readExp i = case parse parseExp "lispy" i of 
  Left err  -> String $ "Oops :/ -> " <> show err
  Right val -> val


symbol :: Parser Char
symbol = oneOf "g!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany space

parseAtom :: Parser LispVal
parseAtom = do 
  head <- letter <|> symbol
  tail <- many (letter <|> digit <|> symbol)
  let atom = head : tail
  pure $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseChar :: Parser Char
parseChar = string "#\\" *> digit 
        <|> string "space" *> char ' '

parseString :: Parser LispVal
parseString = do
  char '"'
  string <- many1 match
  char '"'
  pure . String . concat $ string
    where 
      join a b = [a] <> [b]
      match = (join <$> char '\\' <*> oneOf "nrt\"\\") 
          <|> (:[]) <$> noneOf "\""

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExp
    pure $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = List <$> sepBy parseExp spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExp spaces
  tail <- char '.' *> spaces *> parseExp
  pure $ DottedList head tail

parseExp :: Parser LispVal
parseExp = parseAtom 
       <|> parseString
       <|> parseNumber
       <|> parseQuoted
       <|> do char '('
              x <- try parseList <|> parseDottedList
              char ')'
              pure x
