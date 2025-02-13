module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (h :+ t) lst  = h :+ (t <> lst)
  (<>) (Last el) lst = el :+ lst

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This l) (This r)         = This (l <> r)
  (<>) (This l) (That r)         = Both l r
  (<>) (This l) (Both rl rr)     = Both (rl <> l) rr
  (<>) (That l) (This r)         = Both r l 
  (<>) (That l) (That r)         = That (l <> r)
  (<>) (That l) (Both rl rr)     = Both rl (l <> rr)
  (<>) (Both ll lr) (This r)     = Both (ll <> r) lr
  (<>) (Both ll lr) (That r)     = Both ll (lr <> r)
  (<>) (Both ll lr) (Both rl rr) = Both (ll <> rl) (lr <> rr) 

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) (DS "") r = r
  (<>) l (DS "") = l
  (<>) (DS l) (DS r)    = DS (l ++ "." ++ r)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F l) (F r) = F (l . r) 

instance Monoid (Fun a) where
  mempty = F id
