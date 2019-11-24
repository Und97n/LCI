module Base where

import Text.Printf
import Debug.Trace
import Data.Maybe

data Object =
  Error String |
  Variable String |
  Lambda String Object |
  Application Object Object |
  Clojure String Object Env

data Env = Env [(String, Object)]

instance Show Env where
  show (Env xx) = printf "{%s}" $ pf xx 
    where
      pf :: [(String, Object)] -> String
      pf ((a, b):xs) = printf "%s = %s\n%s" a (show b) $ pf xs
      pf [] = ""


instance Show Object where
  show (Error msg) = printf "ERROR [%s]" msg
  show (Variable a) = a
  show (Application a b) = printf "(%s %s)" (show a) (show b)
  show x =
    case x of
      (Lambda a b) -> printf "$[%s" $ pf a b
      (Clojure a b c) -> printf "C[%s" $ pf a b
      where
        pf :: String -> Object -> String
        pf var1 (Lambda var2 body) = printf "%s,%s" var1 $ pf var2 body
        pf var1 (Clojure var2 body env) = printf "%s,%s" var1 $ pf var2 body
        pf var a = printf "%s].%s" var $ show a
    
envLookup :: Env -> String -> Object
envLookup (Env ((str, fnc):xs)) var = if str == var then fnc else envLookup (Env xs) var
envLookup env str = Error $ printf "variable '%s' not found in scope" str

envInsert :: Env -> String -> Object -> Env
envInsert (Env xx) str val = Env ((str, val) : xx)
