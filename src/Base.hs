module Base where
  import Text.Printf
  import Debug.Trace
  import Data.Maybe


  data Function =
    Error String |
    Variable String |
    Lambda String Function |
    Application Function Function |
    Clojure String Function Env

  data Env = Env [(String, Function)]

  instance Show Env where
    show (Env xx) = printf "{%s}" $ pf xx 
      where
        pf :: [(String, Function)] -> String
        pf ((a, b):xs) = printf "%s = %s\n%s" a (show b) $ pf xs
        pf [] = ""

  instance Show Function where
    show (Error msg) = printf "ERROR [%s]" msg
    show (Variable a) = a
    show (Lambda a b) = printf "$[%s" $ pf a b
      where
        pf :: String -> Function -> String
        pf var1 (Lambda var2 body) = printf "%s,%s" var1 $ pf var2 body
        pf var a = printf "%s].%s" var $ show a
    show (Application a b) = printf "(%s %s)" (show a) (show b)
    show (Clojure a b c) = printf "{#%s.%s}" a (show b)

  envLookup :: Env -> String -> Function
  envLookup (Env ((str, fnc):xs)) var = if str == var then fnc else envLookup (Env xs) var
  envLookup env str = Error $ printf "variable '%s' not found in scope" str

  envInsert :: Env -> String -> Function -> Env
  envInsert (Env xx) str val = Env ((str, val) : xx)