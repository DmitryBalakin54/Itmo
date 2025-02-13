module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S $ mapAnnotated f . g

wrapState :: a -> State s a
wrapState x = S (x :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s ->
  let (S g :# e) = f s
  in g e

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) (S l) (S r) = S $ \s ->
    let (f :# sl) = l s
        (x :# sr) = r sl
    in f x :# sr

instance Monad (State s) where
  (>>=) (S l) f = S $ \s ->
    let (x :# sl) = l s
        (S g)     = f x
    in g sl

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) l r       = Op (Add l r)
  (-) l r       = Op (Sub l r)
  (*) l r       = Op (Mul l r)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val $ fromInteger x

instance Fractional Expr where
  (/) l r        = Op (Div l r)
  fromRational x = Val (fromRational x)  

eval :: Expr -> State [Prim Double] Double
eval (Val x) = wrapState x  
eval (Op op) = case op of
    Add l r  -> evBin Add (+) l r
    Sub l r  -> evBin Sub (-) l r
    Mul l r  -> evBin Mul (*) l r
    Div l r  -> evBin Div (/) l r
    Abs x    -> evUn Abs abs x
    Sgn x    -> evUn Sgn signum x

evBin :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> State [Prim Double] Double
evBin op f l r = do
    ll <- eval l
    rr <- eval r
    modifyState (op ll rr :)
    wrapState (f ll rr)

evUn :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> State [Prim Double] Double
evUn op f x = do
    xx <- eval x
    modifyState (op xx :)
    wrapState (f xx)