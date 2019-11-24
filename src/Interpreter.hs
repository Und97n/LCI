module Interpreter where
import Base
  
import Text.Printf
import Debug.Trace

eval :: Object -> Env -> Object
eval (Lambda a b) env = (Clojure a b env)
eval (Variable str) env = envLookup env str

eval (Application a b) env =
  -- trace (printf "\nA::\t%s\n\t%s\n" (show a) (show a))
  applyF (eval a env) (eval b env)
  where
    applyF :: Object -> Object -> Object
    applyF (Clojure var body clEnv) arg = eval body (envInsert clEnv var arg)

    applyF (ExtCall _ action) arg = action arg env

    applyF (Error msg) _ = Error msg
    applyF a _ = Error $ printf "applying not a function: %s" (show a)


eval a env = a
