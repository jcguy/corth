module Simulator where

import           Corth
import           Data.Char (chr)

data State = State { tokens :: [Token]
                   , stack :: [Integer]
                   , output :: String
                   , isEnd :: Bool
                   }

runtimeError :: Token -> [Char] -> a
runtimeError t msg = error (show t ++ " Runtime error: " ++ msg)

dump :: State -> Maybe State
dump state@(State _ (s:ss) o _) =
  Just $ state { stack = ss, output = o ++ show s ++ "\n" }
dump _ = Nothing

add :: State -> Maybe State
add state@(State _ (a:b:ss) _ _) = Just $ state { stack = (a + b):ss }
add _ = Nothing

sub :: State -> Maybe State
sub state@(State _ (a:b:ss) _ _) = Just $ state { stack = (b - a):ss }
sub _ = Nothing

dup :: State -> Maybe State
dup state@(State _ (a:ss) _ _) = Just $ state { stack = a:a:ss }
dup _ = Nothing

put :: State -> Maybe State
put state@(State _ (a:ss) o _) = Just
  $ state { stack = ss, output = o ++ [chr (fromIntegral a)] }
put _ = Nothing

swap :: State -> Maybe State
swap state@(State _ (a:b:ss) o _) = Just $ state { stack = b:a:ss }
swap _ = Nothing

over :: State -> Maybe State
over state@(State _ (a:b:ss) o _) = Just $ state { stack = b:a:b:ss }
over _ = Nothing

gt :: State -> Maybe State
gt state@(State _ (a:b:ss) _ _) = Just
  $ state { stack = (if b > a
                     then 1
                     else 0)
              :ss
          }
gt _ = Nothing

lt :: State -> Maybe State
lt state@(State _ (a:b:ss) _ _) = Just
  $ state { stack = (if b < a
                     then 1
                     else 0)
              :ss
          }
lt _ = Nothing

while' :: State -> Maybe State
while' s = undefined

if' :: State -> Maybe State
if' s = undefined

else' :: State -> Maybe State
else' s = undefined

end :: State -> Maybe State
end s = undefined

push :: Integer -> State -> Maybe State
push n state@(State _ ss _ _) = Just $ state { stack = n:ss }

step :: State -> State
step s = let result = tryStep s
         in case result of
              Just s  -> s
              Nothing -> runtimeError (head $ tokens s) ""

tryStep :: State -> Maybe State
tryStep (State t@[] s o _) = Just (State t s o True)
tryStep s@(State ((Token t l):ts) _ _ _) =
  let newState = s { tokens = ts }
  in case t of
       OP_DUMP     -> dump newState
       OP_ADD      -> add newState
       OP_SUB      -> sub newState
       OP_DUP      -> dup newState
       OP_PUT      -> put newState
       OP_SWAP     -> swap newState
       OP_OVER     -> over newState
       OP_GT       -> gt newState
       OP_LT       -> lt newState
       BLOCK_WHILE -> while' newState
       BLOCK_IF    -> if' newState
       BLOCK_ELSE  -> else' newState
       BLOCK_END   -> end newState
       VAL_INT n   -> push n newState

simulate :: [Token] -> String
simulate ts =
  simulate' $ State { tokens = ts, stack = [], output = "", isEnd = False }
  where
    simulate' :: State -> String
    simulate' state = case state of
      State _ _ o True      -> o
      s@(State _ _ _ False) -> simulate' $ step s

simulateFile :: FilePath -> IO ()
simulateFile filepath = do
  input <- parseFile filepath
  print
    $ case input of
      Left e   -> show e
      Right xs -> simulate xs