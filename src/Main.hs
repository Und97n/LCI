module Main where
import Base
import Parser
import Interpreter
import Control.Monad (unless)
import Data.Maybe

defaultEnv = Env [
               ("0", seval "$f x.x"),
               ("i", seval "$x.x"),
               ("true", seval "$x y.x"),
               ("false", seval "$x y.y"),
               ("if", seval "$p a b.p a b"),
               ("succ", seval "$n f x. f (n f x)"),
               ("quit", (Variable "quit"))]
           where
             seval str = eval (fromMaybe (Error "can't parse") $ parse str) $ Env []

zeval :: String -> Function
zeval str = eval (fromMaybe (Error "can't parse") $ parse str) defaultEnv

read' :: IO String
read' = getLine

print' :: String -> IO ()
print' = putStrLn

repl :: IO ()
repl = do
  input <- read'
  unless (input == "quit")
       $ print' (show (zeval input))
      >> repl

main = repl
