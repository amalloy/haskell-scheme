module Flatland.Scheme.Evaluator where

import Data.List
import Control.Monad.Error
import Flatland.Scheme.Types

asList :: String -> Value -> Either SchemeException [Value]
asList context Nil = return []
asList context (Cons a d) = return . (a:) =<< asList context d
asList context x = Left $ CompilerException $ ImproperListException context x

asCons :: [Value] -> Value
asCons = foldr Cons Nil

lookupEnv :: Env -> String -> Result
lookupEnv e s = case (lookup s e) of
  Nothing -> Left $ CompilerException $ UnresolvedSymbol s
  Just v -> Right v

eval :: Env -> Value -> Result
eval _ Nil = Right Nil
eval e (Symbol s) = lookupEnv e s
eval e (Lambda f source) = Left $ RuntimeException $ TypeError [NilType, ConsType, SymbolType] LambdaType
eval e c@(Cons a d) =
  let resolve = lookupEnv e
  in case a of
    (Symbol "quote") -> return $ car d
    (Symbol "lambda") -> evalLambda e d
    (Symbol "if") -> do
      body <- asList "'if expression" d
      case body of
        [test, t, f] -> do
          v <- eval e test
          eval e (if v == Nil then f else t)
        otherwise -> Left $ RuntimeException $ ArityException 3 (length body)
    otherwise -> do
      argList <- (asList "arguments to lambda" c)
      (f:args) <- forM argList (eval e)
      case f of
        (Lambda f source) -> f args
        otherwise -> Left $ RuntimeException $ TypeError [LambdaType] (typeOf f)

withEnv :: [String] -> [Value] -> Env -> Either SchemeException Env
withEnv params args e | (paramCount == argCount) = return $ (params `zip` args) ++ e
                      | otherwise = Left (RuntimeException $ ArityException paramCount argCount)
  where paramCount = length params
        argCount = length args

evalLambda :: Env -> Value -> Result
evalLambda e fnbody = do
  [arglist,body] <- asList "lambda body" fnbody
  params <- asList "lambda parameter list" arglist
  let f args = (withEnv (map name params) args e) >>= (flip eval) body
  return $ Lambda f (Cons (Symbol "lambda") fnbody)


nativeCode :: Value
nativeCode = asCons $ map Symbol ["native", "code"]

arityException :: Int -> Int -> SchemeException
arityException expected actual = RuntimeException $ ArityException expected actual

schemeFn :: String -> (Value -> Result) -> Value
schemeFn name f = Lambda g nativeCode
  where g [x] = f x
        g args = Left $ arityException 1 (length args)

schemeFn2 :: String -> (Value -> Value -> Result) -> Value
schemeFn2 name f = Lambda g nativeCode
  where g [x, y] = f x y
        g args = Left $ arityException 2 (length args)

sEq :: Value
sEq = schemeFn2 "eq?" eq
  where eq x y = return $ if x == y then (Symbol "t") else Nil

sCons :: Value
-- sCons = schemeFn2 "cons" (return .) . Cons -- this would work, but confuses me
sCons = schemeFn2 "cons" cons
  where cons a d = return $ Cons a d

sCar :: Value
sCar = schemeFn "car" car
  where car (Cons a d) = return a
        car x = fail $ "Can't get car of non-cons: " ++ (show x)

sCdr :: Value
sCdr = schemeFn "cdr" cdr
  where cdr (Cons a d) = return d
        cdr x = fail $ "Can't get cdr of non-cons: " ++ (show x)

initialEnv :: Env
initialEnv = [("eq?", sEq), ("cons", sCons), ("car", sCar), ("cdr", sCdr)]