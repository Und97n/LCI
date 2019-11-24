module Interpreter where
import Base
  
import Text.Printf
import Debug.Trace

applyF :: Object -> Object -> Object

applyF (Clojure var body clEnv) arg =
  trace (printf "INS: %s=%s" var (show arg))
  eval body (envInsert clEnv var arg)
applyF a b = Error $ printf "applying not a function: %s" (show a)

eval :: Object -> Env -> Object
eval (Lambda a b) env = (Clojure a b env)
eval (Variable str) env = envLookup env str

eval (Application a b) env =
  -- trace (printf "\nA::\t%s\n\t%s\n" (show a) (show a))
  let ra = eval a env
      rb = eval b env
  in applyF ra rb

eval a env = a
