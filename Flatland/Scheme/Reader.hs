module Flatland.Scheme.Reader where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Flatland.Scheme.Value

parseValue :: CharParser () Value
parseValue = try parseList <|> parseNil <|> parseSymbol

parseNil :: CharParser () Value
parseNil = (string "nil") >> return Nil

parseSymbol :: CharParser () Value
parseSymbol = liftM Symbol (many1 (noneOf " \n\t()"))

whitespace = (oneOf " \n\t")

parseList :: CharParser () Value
parseList = do
  many whitespace
  char '('
  many whitespace
  exprs <- parseValue `sepEndBy` (many whitespace)
  char ')'
  many whitespace
  return $ foldr Cons Nil exprs
