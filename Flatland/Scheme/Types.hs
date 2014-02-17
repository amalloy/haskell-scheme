{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Flatland.Scheme.Types where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)

data CompilerException = UnresolvedSymbol Value
                       | ImproperListException {context :: String, value :: Value}
instance Show CompilerException where
  show (UnresolvedSymbol s) = "Unable to resolve symbol " ++ show s
  show (ImproperListException context value) = "Expected proper list for " ++
                                               context ++ ", but got " ++ show value

data RuntimeException = TypeError [SchemeType] SchemeType -- expected, actual
                      | ArityException Int Int
instance Show RuntimeException where
  show (TypeError [expected] actual) = "Type error: expected " ++ show expected ++
                                       ", but had " ++ show actual
  show (TypeError expected actual) = "Type error: expected any of " ++ show expected ++
                                     ", but had " ++ show actual
  show (ArityException expected actual) = "Arity mismatch: needed " ++ show expected ++
                                          " args, but got " ++ show actual

data SchemeException = ReaderException ParseError
                     | CompilerException CompilerException
                     | RuntimeException RuntimeException
                     deriving Show

type Result = Either SchemeException Value

data SchemeType = NilType | ConsType | SymbolType | LambdaType
instance Show SchemeType where
  show NilType = "nil"
  show ConsType = "list"
  show SymbolType = "symbol"
  show LambdaType = "compiled function"

type Env = Value -- a list of (Cons Symbol Value) elements
type Lambda = [Value] -> Result

instance Eq Lambda where { _ == _ = False }

data Value = Nil | Cons {car::Value, cdr::Value}
           | Lambda {fn::Lambda, source::Value}
           | Symbol {name::String}
           deriving Eq

instance Show Value where
  show Nil = "nil"
  show (Symbol s) = s
  show (Lambda f source) = show source
  show c@(Cons a d) = "(" ++ unwords (showList c) ++ ")"
    where showList (Cons a Nil) = [show a]
          showList (Cons a d@(Cons _ _)) = show a : showList d
          showList (Cons a d) = show a : "." : [show d]

typeOf :: Value -> SchemeType
typeOf Nil = NilType
typeOf (Cons _ _) = ConsType
typeOf (Symbol _) = SymbolType
typeOf (Lambda _ _) = LambdaType
