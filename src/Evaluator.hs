module Evaluator where

import Types

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List (Atom f : args)) = apply f $ eval <$> args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", isSymbol)
             , ("boolean?", isBoolean)
             ]

--TODO
isSymbol :: [LispVal] -> LispVal
isSymbol val = if length val > 1
                   then undefined
                   else case head val of
                          Bool _ -> Bool True 
                          _      -> Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean val = if length val > 1
                   then undefined
                   else case head val of
                          Bool _ -> Bool True 
                          _      -> Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ unpackNum <$> params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ head parsed 
unpackNum _ = 0
