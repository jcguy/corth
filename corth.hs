{- HLINT ignore "Use lambda-case" -}
module Main where

import           Data.Map (Map, withoutKeys)
import           Text.Read (readMaybe)
import           Debug.Trace (trace)
import           Control.Applicative (Alternative(empty, (<|>)))
import           Data.Char (isDigit)

debug :: c -> String -> c
debug = flip trace

data Token =
    OP_DUMP
  | OP_ADD
  | OP_SUB
  | OP_DUP
  | OP_PUT
  | OP_SWAP
  | OP_OVER
  | OP_GT
  | OP_LT
  | BLOCK_WHILE
  | BLOCK_IF
  | BLOCK_ELSE
  | BLOCK_END
  | VAL_INT Integer
  deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser
    $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p1) (Parser p2) = Parser
    $ \input -> do
      (input', f) <- p1 input
      (input'', x) <- p2 input'
      Just (input'', f x)

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ const Nothing

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (y:ys)
      | y == c = Just (ys, c)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser
  $ \input -> let (token, rest) = span f input
              in Just (rest, token)

intP :: Parser Token
intP = f <$> notEmpty (spanP isDigit)
  where
    f ds = VAL_INT $ read ds

  -- where f ds = VAL_INT $ readMaybe ds
notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) = Parser
  $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

keywordParser :: (Token, String) -> Parser Token
keywordParser (t, s) = t <$ stringP s

keywordParsers :: [Parser Token]
keywordParsers = map
  keywordParser
  [ (OP_DUMP, ".")
  , (OP_ADD, "+")
  , (OP_SUB, "-")
  , (OP_DUP, "dup")
  , (OP_PUT, "put")
  , (OP_SWAP, "swap")
  , (OP_OVER, "over")
  , (OP_GT, ">")
  , (OP_LT, "<")
  , (BLOCK_WHILE, "while")
  , (BLOCK_IF, "if")
  , (BLOCK_ELSE, "else")
  , (BLOCK_END, "end")]

token :: Parser Token
token = foldl1 (<|>) keywordParsers <|> intP

parse :: String -> Maybe [Token]
parse s = traverse (f . runParser token) (words s)
  where
    f :: Maybe (String, Token) -> Maybe Token
    f m = snd <$> m

main :: IO ()
main = print
  $ parse "\
  \3 4 + .\n\
  \12 -1 + .\n\
  \8 4 - .\n\
  \4 2 - .\n"