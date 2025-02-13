module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , concatinateList
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some x, Some y) = Some (x, y)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P xl yl, P xr yr) = P (xl, xr) (yl, yr)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q xl yl zl wl, Q xr yr zr wr) = Q (xl, xr) (yl, yr) (zl, zr) (wl, wr)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

-- You may add necessary constraints here
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (al :# el, ar :# er) = (al, ar) :# (el <> er)

-- You may add necessary constraints here
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)             = Error e
distExcept (_, Error e)             = Error e
distExcept (Success xl, Success xr) = Success (xl, xr)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, Low y)       = Low (x, y)
distPrioritised (Low x, Medium y)    = Medium (x, y)
distPrioritised (Low x, High y)      = High (x, y)
distPrioritised (Medium x, Low y)    = Medium (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (Medium x, High y)   = High (x, y)
distPrioritised (High x, Low y)      = High (x, y)
distPrioritised (High x, Medium y)   = High (x, y)
distPrioritised (High x, High y)     = High (x, y)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (xl :> xls, xr :> xrs) = (xl, xr) :> distStream (xls, xrs)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x


concatinateList :: List a -> List a -> List a
concatinateList Nil r        = r
concatinateList (l :. Nil) r = l :. r
concatinateList (l :. ls) r  = l :. concatinateList ls r


distList :: (List a, List b) -> List (a, b)
distList (Nil, _)               = Nil
distList (_, Nil)               = Nil
distList (xl :. xls, xr :. xrs) = concatinateList left right
  where 
    left  = mapList (\x -> (xl, x)) (xr :. xrs) 
    right = distList (xls, xr :. xrs)

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapFun :: a -> Fun i a
wrapFun v = F (const v)
