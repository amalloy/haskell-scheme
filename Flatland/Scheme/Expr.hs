module Flatland.Scheme.Expr where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List

data Expr = Nil | Symbol String | List [Expr]
  deriving (Eq)

instance Show Expr where
  show Nil = "()"
  show (Symbol s) = s
  show (List xs) = "(" ++ (intercalate " " $ map show xs) ++ ")"

parseExpr :: CharParser () Expr
parseExpr = try parseList <|> parseNil <|> parseSymbol

parseNil :: CharParser () Expr
parseNil = (string "nil") >> return Nil

parseSymbol :: CharParser () Expr
parseSymbol = liftM Symbol (many1 (noneOf " \n\t()"))

whitespace = (oneOf " \n\t")

parseList :: CharParser () Expr
parseList = do
  many whitespace
  char '('
  many whitespace
  exprs <- parseExpr `sepEndBy` (many whitespace)
  char ')'
  many whitespace
  return $ List exprs
