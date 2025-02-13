module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1
import HW3.T2 (concatinateList)

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some x)) = Some x
joinOption (Some None)     = None
joinOption None            = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success x) = x
joinExcept (Error x)   = Error x

-- You may add necessary constraints here
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# el) :# er) = x :# (er <> el)

joinList :: List (List a) -> List a
joinList Nil       = Nil
joinList (x :. xs) = concatinateList x (joinList xs)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> runFun (f i) i)
  where
    runFun (F g) = g