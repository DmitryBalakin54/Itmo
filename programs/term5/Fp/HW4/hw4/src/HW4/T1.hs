module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> case runES es s of
  Success (a :# e)  -> Success (f a :# e)
  Error e           -> Error e

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \s -> case runES es s of
  Success (b :# ee) -> runES b ee
  Error e           -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success(() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) s@(ES _) g = joinExceptState $ fmap g s

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x  
eval (Op op) = case op of
    Add l r  -> evBin Add (+) l r
    Sub l r  -> evBin Sub (-) l r
    Mul l r  -> evBin Mul (*) l r
    Div l r  -> evDiv l r
    Abs x    -> evUn Abs abs x
    Sgn x    -> evUn Sgn signum x

evDiv :: Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evDiv l r = do
  ll <- eval l
  rr <- eval r
  if rr == 0
    then throwExceptState DivideByZero
    else do
      modifyExceptState (\s -> Div ll rr : s)
      return (ll / rr)

evBin :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evBin op f l r = do
    ll <- eval l
    rr <- eval r
    modifyExceptState (\s -> op ll rr : s)
    return (f ll rr)

evUn :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
evUn op f x = do
    xx <- eval x
    modifyExceptState (\s -> op xx : s)
    return (f xx)