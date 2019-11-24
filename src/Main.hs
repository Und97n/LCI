module Main where
import Base
import Parser
import Interpreter
import Control.Monad (unless)
import Data.Maybe
import System.IO

numberPrinterO = ExtCall "printnum" numberPrinter
numberPrinter :: Object -> Env -> Object
numberPrinter (Error x) _ = Error (show ((read x :: Int) + 1))
numberPrinter x env = eval (Application (Application x numberPrinterO) (Error "0")) env 

-- factorial = fixed ($f n.(iszero n) 1 (mult n (f (pred n))))

defaultEnv = Env [
               ("0", seval "$f x.x"),
               ("1", seval "$f x. f x"),
               ("2", seval "$f x. f (f x)"),
               ("3", seval "$f x. f (f (f x))"),
               ("4", seval "$f x. f (f (f (f x)))"),
               ("iszero", seval "$n.(n ($a x y.y) ($x y.x))"),
               ("plus", seval "$m n.($f x. m f (n f x))"),
               ("mult", seval "$m n.($f. m (n f))"),
               ("pow", seval "$b e. e b"),
               ("i", seval "$x.x"),
               ("true", seval "$x y.x"),
               ("false", seval "$x y.y"),
               ("if", seval "$p a b.p a b"),
               ("succ", seval "$n f x. f (n f x)"),
               ("fixed", seval "$f.(($x.f (x x)) ($x.f (x x)))"),
               ("pred", seval "$n f x. n ($ g h. h (g f)) ($u.x) ($u.u)"),
               ("printnum", numberPrinterO)]
           where
             seval str = eval (fromMaybe (Error "can't parse") $ parse str) $ Env []

zeval :: String -> Object
zeval str = eval (fromMaybe (Error "can't parse") $ parse str) defaultEnv

read' :: IO String
read' = putStr "> "
     >> hFlush stdout
     >> getLine

print' :: String -> IO ()
print' = putStrLn

repl :: IO ()
repl = do
  input <- read'
  unless (input == "quit")
       $ print' (show (zeval input))
      >> repl

main = repl
