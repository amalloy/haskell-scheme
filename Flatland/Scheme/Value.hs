module Flatland.Scheme.Value where

data Value = Nil | Cons Value Value | Lambda (Value -> Value) | Symbol
