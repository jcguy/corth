{- HLINT ignore "Use lambda-case" -}
module Main where

import           Data.Map (Map, withoutKeys)
import           Text.Read (readMaybe)
import           Debug.Trace (trace)
import           Control.Applicative (Alternative(empty, (<|>)))
import           Data.Char (isDigit, isAlphaNum, isSpace, isSymbol)
import           GHC.Integer (integerToInt)

debug :: c -> String -> c
debug = flip trace

data TokenType =
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

data Loc = Loc { file :: String, row :: Integer, col :: Integer }

instance Show Loc where
  show (Loc file row col) = file <> ":" <> show row <> ":" <> show col

data Token = Token { type' :: TokenType, loc :: Loc }

instance Show Token where
  show (Token type' loc) = "\n" <> show loc <> "\t" <> show type'

data CorthWord = CorthWord { str :: String, wordLoc :: Loc }
  deriving (Show)

wordToToken :: TokenType -> CorthWord -> Token
wordToToken t w = Token t $ wordLoc w

newtype Parser a =
  Parser { runParser :: [CorthWord] -> Maybe ([CorthWord], a) }

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

stringP :: String -> Parser CorthWord
stringP s = Parser f
  where
    f [] = Nothing
    f (cw:cws)
      | s == str cw = Just (cws, cw)
      | otherwise = Nothing

notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) = Parser
  $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

keywordParser :: (TokenType, String) -> Parser Token
keywordParser (t, s) = Parser
  $ \input -> do
    (input', x) <- runParser (stringP s) input
    Just (input', wordToToken t x)

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

intParser :: Parser Token
intParser = Parser
  $ \(x:xs) -> let result = readMaybe $ str x :: Maybe Integer
               in case result of
                    Just n -> Just (xs, wordToToken (VAL_INT n) x)
                    _      -> Nothing
                      `debug` ("Couldn't read \"" <> str x <> "\" as Integer")

token :: Parser Token
token = foldl1 (<|>) keywordParsers <|> intParser

parse :: [CorthWord] -> Maybe [Token]
parse input = sequenceA $ parse' input
  where
    parse' :: [CorthWord] -> [Maybe Token]
    parse' [] = []
    parse' (x:xs) = (snd <$> runParser token [x]):parse' xs

lexFile :: String -> String -> [CorthWord]
lexFile filename = lex' filename 1 1
  where
    lex' :: String -> Integer -> Integer -> String -> [CorthWord]
    lex' _ row col [] = []
    lex' file row col xs
      | '\n' == head xs = lex' file (1 + row) 1 (tail xs)
      | isSpace $ head xs = lex' file row (1 + col) (tail xs)
      | otherwise = let word = takeWhile (not . isSpace) xs
                        n = length word
                    in CorthWord word (Loc file row col)
                       :lex' file row (col + toInteger n) (drop n xs)

parseFile :: FilePath -> IO (Maybe [Token])
parseFile filename = do
  input <- readFile filename
  return $ parse $ lexFile filename input

main :: IO ()
main = undefined