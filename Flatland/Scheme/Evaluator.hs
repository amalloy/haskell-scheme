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

lookupEnv :: Env -> Value -> Result
lookupEnv (Cons (Cons (Symbol a) v) e) s@(Symbol b) | a == b = Right v
                                                    | otherwise = lookupEnv e s
lookupEnv Nil s = Left $ CompilerException $ UnresolvedSymbol s
lookupEnv (Cons (Cons x _) e) s = Left $ RuntimeException $ TypeError [SymbolType] (typeOf x)
lookupEnv (Cons x e) s = Left $ RuntimeException $ TypeError [ConsType] (typeOf x)
lookupEnv x s = Left $ RuntimeException $ TypeError [ConsType] (typeOf x)

eval :: Env -> Value -> Result
eval _ Nil = return Nil
eval e s@(Symbol _) = lookupEnv e s
eval e (Lambda f source) = Left $ RuntimeException $ TypeError [NilType, ConsType, SymbolType] LambdaType
eval e c@(Cons a d) =
  case a of
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
      -- note we're not handling [] at all - that case is impossible, because we already know we have a Cons
      case f of
        (Lambda f source) -> f args
        otherwise -> Left $ RuntimeException $ TypeError [LambdaType] (typeOf f)

withEnv :: [Value] -> [Value] -> Env -> Either SchemeException Env
withEnv params args e | (paramCount == argCount) = return $ foldr Cons e $
                                                   zipWith Cons params args
                      | otherwise = Left (RuntimeException $ ArityException paramCount argCount)
  where paramCount = length params
        argCount = length args

evalLambda :: Env -> Value -> Result
evalLambda e fnbody = do
  [arglist,body] <- asList "lambda body" fnbody
  params <- asList "lambda parameter list" arglist
  let f args = (withEnv params args e) >>= (flip eval) body
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
initialEnv = asCons $ map (\(s, x) -> (Cons (Symbol s) x)) $
             [("eq?", sEq), ("cons", sCons), ("car", sCar), ("cdr", sCdr)]
