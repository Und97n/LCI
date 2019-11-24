module Interpreter where
import Base
  
import Text.Printf
import Debug.Trace

applyF :: Object -> Object -> Object

applyF (Function var body clEnv) arg =
  trace (printf "INS: %s=%s" var (show arg))
  eval body (envInsert clEnv str rb)
applyF a b = Error "applying not a function: %s"

eval :: Object -> Env -> Object
eval (Lambda a b) env = (Function a b env)
eval (Variable str) env = envLookup env str

eval (Application a b) env =
  trace (printf "\nA::\t%s\n\t%s\n" (show ra) (show rb))
  applyF (eval a) (eval b)

eval a env = a
