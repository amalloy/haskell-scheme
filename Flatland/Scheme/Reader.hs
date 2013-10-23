module Flatland.Scheme.Reader (read, readEval) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Flatland.Scheme.Types
import Flatland.Scheme.Value
import Prelude hiding (read)

parseValue :: CharParser () Value
parseValue = try parseList <|> parseQuote <|> parseNil <|> parseSymbol

parseQuote :: CharParser () Value
parseQuote = do
  char '\''
  many whitespace
  e <- parseValue
  many whitespace
  return $ Cons (Symbol "quote") (Cons e Nil)

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

read :: String -> Either ParseError Value
read = runParser parseValue () "stdin"

-- this function doesn't belong here long-term, but is useful for playing around
readEval :: String -> Result
readEval s =
  case (read s) of
    (Left e) -> Left (ReaderException e)
    (Right v) -> eval initialEnv v
