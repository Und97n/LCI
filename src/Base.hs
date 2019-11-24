module Base where

import Text.Printf
import Debug.Trace
import Data.Maybe

data Object =
  Error String |
  Variable String |
  Lambda String Object |
  Application Object Object |
  Clojure String Object Env |
  ExtCall String (Object -> Env -> Object)

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
  show (ExtCall name _) = printf "@[%s]" name  
  show (Application a b) = printf "(%s %s)" (pa a) (show b)
    where
      pa :: Object -> String
      pa (Application a b) = printf "%s %s" (pa a) (show b)
      pa x = show x
  show x =
    case x of
      (Lambda a b) -> printf "$%s" $ pf a b
      (Clojure a b c) -> printf "#%s" $ pf a b
      where
        pf :: String -> Object -> String
        pf var1 (Lambda var2 body) = printf "%s %s" var1 $ pf var2 body
        pf var1 (Clojure var2 body env) = printf "%s %s" var1 $ pf var2 body
        pf var a = printf "%s.%s" var $ show a
    
envLookup :: Env -> String -> Object
envLookup (Env ((str, fnc):xs)) var = if str == var then fnc else envLookup (Env xs) var
envLookup env str = Error $ printf "variable '%s' not found in scope" str

envInsert :: Env -> String -> Object -> Env
envInsert (Env xx) str val = Env ((str, val) : xx)
