module Flatland.Scheme.Value where

import Data.List

type Env = [(String, Value)]

data Value = Nil | Cons {car::Value, cdr::Value}
           | Lambda ([Value] -> Value) Value
           | Symbol String

instance Eq Value where
  Nil == Nil = True
  Nil == _ = False
  (Cons a d) == (Cons a' d') = a == a' && d == d'
  (Cons _ _) == _ = False
  (Lambda _ _) == _ = False
  (Symbol a) == (Symbol b) = a == b

instance Show Value where
  show Nil = "()"
  show (Symbol s) = s
  show (Lambda f source) = show source
  show c@(Cons a d) = "(" ++ (intercalate " " $ showList c) ++ ")"
    where showList (Cons a Nil) = [(show a)]
          showList (Cons a d@(Cons _ _)) = show a : showList d
          showList (Cons a d) = show a : "." : [show d]

asList :: Value -> [Value]
asList Nil = []
asList (Cons a d) = a:asList d

asCons :: [Value] -> Value
asCons = foldr Cons Nil

lookupEnv :: Env -> String -> Either String Value
lookupEnv e s = case (lookup s e) of
  Nothing -> Left $ "Unable to resolve symbol: " ++ s
  Just v -> Right v

eval :: Env -> Value -> Either String Value
eval _ Nil = Right Nil
eval e (Symbol s) = lookupEnv e s
eval e (Lambda f source) = Left $ "Can't eval a function: " ++ show source
eval e (Cons a d) =
  let (Symbol s) = a
      resolve = lookupEnv e
  in case s of
    "quote" -> Right $ car d
    "lambda" -> undefined -- this is the hard case
    "eq" -> do
      let (Cons x (Cons y Nil)) = d
      a <- eval e x
      b <- eval e y
      return $ if a == b then (Symbol "t") else Nil
    "if" -> do
      let (Cons test (Cons t (Cons f Nil))) = d
      v <- eval e test
      eval e (if v == Nil then f else t)
