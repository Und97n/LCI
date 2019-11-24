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
                  
parseVariable = do s <- parseString
                   whitespaces
                   return (Variable s)

parseLambda = do char '$'
                 whitespaces
                 vars <- sepBy1 parseString whitespaces1
                 whitespaces
                 char '.'
                 b <- parseProgram
                 whitespaces
                 return $ foldr (\x acc -> Lambda x acc) b vars

parseApplication = do lst <- sepBy1 (parseVariable +++ brackets parseProgram) whitespaces1
                      whitespaces
                      return $ foldl1 (\acc x -> (Application acc x)) lst

parseProgram = do
  whitespaces
  parseLambda +++ parseApplication

parse :: String -> (Maybe Object)

parse str =
  let lst = [ x | (x,"") <- readP_to_S parseProgram str]
  in if (null lst) then Nothing else Just $ head lst
