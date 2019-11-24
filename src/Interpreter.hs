module Interpreter where
  import Base
  
  import Text.Printf
  import Debug.Trace

  eval :: Function -> Env -> Function
  eval (Lambda a b) env = (Clojure a b env)
  eval (Variable str) env = envLookup env str
  eval (Application a b) env =
    trace (printf "\nA::\t%s\n\t%s\n" (show ra) (show rb)) eeval ra
    where
      ra = eval a env
      rb = eval b env
      eeval (Clojure str body (Env xx)) =
        trace (printf "INS: %s=%s" str (show rb)) eval body (envInsert env str rb)
      eeval (Error a) = Error a
      eeval a = (Error $ printf "Unexpected in eeval: %s" (show a))

  eval a env = a
