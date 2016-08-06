module Tokenizer ( Tokenizer
                 , Token (..)
                 , nextInt
                 , nextRational
                 , nextToken
                 ) where

import Control.Monad.State
import Data.Char
import Data.Ratio

data Token = Number Integer
           | Comma
           | Slash
           | Minus
           | EOF
             deriving (Show, Eq)

type Tokenizer = State String

eatToken :: String -> (Token, String)
eatToken [] = (EOF, [])
eatToken css@(c:cs) | isSpace c = eatToken cs
                    | isDigit c = (Number $ read ts, hs)
                    | c == ',' = (Comma, cs)
                    | c == '/' = (Slash, cs)
                    | c == '-' = (Minus, cs)
                    where (ts, hs) = span isDigit css

ungetToken :: Token -> String -> String
ungetToken (Number n) s = show n ++ s
ungetToken Comma s = ',':s
ungetToken Slash s = '/':s
ungetToken Minus s = '-':s
ungetToken EOF s = s

nextToken :: Tokenizer Token
nextToken = state eatToken

nextInt :: Tokenizer Int
nextInt = do
  (Number n) <- nextToken
  return $ fromIntegral n

nextRational :: Tokenizer Rational
nextRational = do
  leading <- nextToken
  case leading of
    Minus -> do r <- nextRational
                return $ -r
    _     -> do modify (ungetToken leading)
                (Number n) <- nextToken
                sep <- nextToken
                case sep of
                  Slash -> do (Number d) <- nextToken
                              return $ n % d
                  _     -> do modify (ungetToken sep)
                              return $ n % 1
