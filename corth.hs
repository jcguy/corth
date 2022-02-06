module Main where

import           Data.Map (Map)
import           Text.Read (readMaybe)

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
  | OP_EQ
  | BLOCK_WHILE
  | BLOCK_IF
  | BLOCK_ELSE
  | BLOCK_END
  | VALUE_INT Integer
  deriving (Show, Eq)

data Location = Location { filename :: String, row :: Integer, col :: Integer }
  deriving (Eq)

instance Show Location where
  show (Location file row col) =
    show file ++ ":" ++ show row ++ ":" ++ show col

data Token = Token { tokenType :: TokenType, tokenLoc :: Location }
  deriving (Eq)

instance Show Token where
  show (Token type' loc) = show loc ++ " " ++ show type'

data Input = Input { inputLoc :: Location, inputStr :: String }
  deriving (Eq, Show)

inputPop :: Input -> Maybe (Char, Input)
inputPop (Input _ []) = Nothing
inputPop
  (Input loc (x:xs)) = Just (x, Input { inputLoc = newLoc, inputStr = xs })
  where
    newLoc = case x of
      '\n' -> loc { row = row loc + 1, col = 0 }
      _    -> loc { col = col loc + 1 }

data ParserError = ParserError Location String
  deriving (Show)

newtype Parser a =
  Parser { runParser :: Input -> Either ParserError (Input, a) }

charP :: Char -> Parser Char
charP x = Parser $ g . f
  where
      f :: Input -> (Location, Maybe (Char, Input))
      f (Input loc str) = (loc, inputPop $ Input loc str)
      g :: (Location, Maybe (Char, Input)) -> Either ParserError (Input, Char)
      g (loc, result) = case result of
          Just (y, ys) -> z where
              z
                | x == y = Right (ys, y)
                | otherwise = Left $ ParserError loc ("Expected '" ++ [x] ++ "' but got '" ++ [y] ++ "'")
          Nothing -> Left $ ParserError loc ("Expected '" ++ [x] ++ "' but reached end of string")

stringP :: String -> Parser String
stringP = undefined

parse :: String -> [Token]
parse ss = map parse' $ words ss

parse' :: String -> Token
parse' word =
  Token { tokenType = case word of
            "."    -> OP_DUMP
            "+"    -> OP_ADD
            "-"    -> OP_SUB
            "dup"  -> OP_PUT
            "put"  -> OP_PUT
            "swap" -> OP_SWAP
            "over" -> OP_OVER
            ">"    -> OP_GT
            "<"    -> OP_LT
            _      -> case readMaybe word of
              Just n  -> VALUE_INT n
              Nothing -> error $ "unknown word " ++ word
        , tokenLoc = Location "here.corth" 0 0
        }

main :: IO ()
main = do
  print $ parse "3 4 + .\n\
   \12 -1 + .\n\
   \8 4 - .\n\
   \4 2 - .\n"
