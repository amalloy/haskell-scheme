module Flatland.Scheme.Value where

import Data.List

type Env = [(String, Value)]

data Value = Nil | Cons {car::Value, cdr::Value}
           | Lambda (Value -> Value) Value
           | Symbol String

instance Show Value where
  show Nil = "()"
  show (Symbol s) = s
  show (Lambda f source) = show source
  show c@(Cons a d) = "(" ++ (intercalate " " $ showList c) ++ ")"
    where showList (Cons a Nil) = [(show a)]
          showList (Cons a d@(Cons _ _)) = show a : showList d
          showList (Cons a d) = show a : "." : [show d]
