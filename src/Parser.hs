module Parser where
  import Text.Printf
  import Text.ParserCombinators.ReadP
  import Debug.Trace
  import Data.Maybe

  import Base
  
  whitespaces = do many (choice (map char [' ','\n', '\t']))
                   return ()

  whitespaces1 = do many1 (choice (map char [' ','\n', '\t']))
                    return ()

  brackets p = do whitespaces
                  char '('
                  r <- p
                  whitespaces
                  char ')'
                  return r

  parseString = many1 (choice (map char (['a'..'z'] ++ ['0' .. '9'])))
                  
  parseVariable = do whitespaces
                     s <- parseString
                     return (Variable s)

  parseLambda = do whitespaces
                   char '$'
                   vars <- sepBy parseString whitespaces1 
                   char '.'
                   b <- parseProgram
                   return $ foldr (\x acc -> Lambda x acc) b vars

  parseApplication = do whitespaces
                        lst <- sepBy (parseVariable +++ brackets parseProgram) whitespaces1
                        return $ foldl1 (\acc x -> (Application acc x)) lst

  parseProgram = parseLambda +++ parseApplication

  parse :: String -> (Maybe Function)

  parse str = if (null lst) then Nothing else Just $ head lst
    where
      lst = [ x | (x,"") <- readP_to_S parseProgram str]