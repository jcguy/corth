{-# LANGUAGE FlexibleInstances #-}

{- HLINT ignore "Use lambda-case" -}
module Corth where

import           Text.Read (readMaybe)
import           Control.Applicative (Alternative(empty, (<|>)))
import           Data.Char (isSpace)

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

instance Eq Loc where
  (==) (Loc f1 r1 c1) (Loc f2 r2 c2) = f1 == f2 && r1 == r2 && c1 == c2

instance Ord Loc where
  compare (Loc f1 r1 c1) (Loc f2 r2 c2)
    | f1 /= f2 = compare f1 f2
    | r1 /= r2 = compare r1 r2
    | c1 /= c2 = compare c1 c2
    | otherwise = EQ

data Token = Token { type' :: TokenType, loc :: Loc }

instance Show Token where
  show (Token type' loc) = "\n" <> show loc <> "\t" <> show type'

data CorthWord = CorthWord { str :: String, wordLoc :: Loc }
  deriving (Show)

wordToToken :: TokenType -> CorthWord -> Token
wordToToken t w = Token t $ wordLoc w

data ParserError = UnknownError
                 | KnownError { errLoc :: Loc, errStr :: String }

instance Show ParserError where
  show UnknownError = "Unknown error encountered"
  show (KnownError l s) = show l ++ " " ++ s

newtype Parser a =
  Parser { runParser :: [CorthWord] -> Either ParserError ([CorthWord], a) }

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser
    $ \input -> do
      (input', x) <- p input
      Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p1) (Parser p2) = Parser
    $ \input -> do
      (input', f) <- p1 input
      (input'', x) <- p2 input'
      Right (input'', f x)

instance Alternative (Either ParserError) where
  empty = Left UnknownError

  (<|>) (Left UnknownError) e2 = e2
  (<|>) e1@(Left (KnownError loc1 _)) e2@(Left (KnownError loc2 _))
    | loc2 > loc1 = e2
    | otherwise = e1
  (<|>) (Left _) (Right x) = Right x
  (<|>) e1 _ = e1

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ const $ Left UnknownError

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input

stringP :: String -> Parser CorthWord
stringP s = Parser f
  where
    f [] = Left UnknownError
    f (cw:cws)
      | s == str cw = Right (cws, cw)
      | otherwise = Left
        $ KnownError (wordLoc cw) ("Unknown word \"" ++ str cw ++ "\"")

keywordParser :: (TokenType, String) -> Parser Token
keywordParser (t, s) = Parser
  $ \input -> do
    (input', x) <- runParser (stringP s) input
    Right (input', wordToToken t x)

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
  $ \(x:xs)
  -> let result = readMaybe $ str x :: Maybe Integer
     in case result of
          Just n -> Right (xs, wordToToken (VAL_INT n) x)
          _      -> Left
            $ KnownError (wordLoc x) ("Unknown word \"" <> str x <> "\"")

token :: Parser Token
token = foldl1 (<|>) keywordParsers <|> intParser

parse :: [CorthWord] -> Either ParserError [Token]
parse input = sequenceA $ parse' input
  where
    parse' :: [CorthWord] -> [Either ParserError Token]
    parse' [] = []
    parse' (x:xs) = let result = (snd <$> runParser token [x])
                    in case result of
                         Right r -> Right r:parse' xs
                         Left e  -> [Left e]

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

parseFile :: FilePath -> IO (Either ParserError [Token])
parseFile filename = do
  input <- readFile filename
  return $ parse $ lexFile filename input

main :: IO ()
main = undefined
