{-# OPTIONS_GHC -Wno-unused-matches #-}
module Data.ListZipper
  ( ListZipper (..)
  , shift
  , truncateLZ
  , shiftLeft
  , shiftRight
  , insert
  ) where

import Control.Comonad (Comonad (..))
import Data.List (unfoldr)


data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  extend f lz@(LZ ls x rs) =
    LZ (unfoldr (fmap (\lz'' -> (f lz'', lz'')) . moveLeft) lz)
       (f lz)
       (unfoldr (fmap (\lz'' -> (f lz'', lz'')) . moveRight) lz)


moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (LZ (l:ls) x rs) = Just (LZ ls l (x:rs))
moveLeft _                = Nothing

moveRight :: ListZipper a -> Maybe (ListZipper a)
moveRight (LZ ls x (r:rs)) = Just (LZ (x:ls) r rs)
moveRight _                = Nothing

apply :: (a -> a) -> a -> [a]
apply f = tail . iterate f

shift :: (a -> a) -> (a -> a) -> a -> ListZipper a
shift f g e = LZ (apply f e) e (apply g e)

truncateLZ :: Int -> ListZipper a -> [a]
truncateLZ cnt (LZ left el right) =
  reverse (take cnt left) ++ [el] ++ take cnt right

shiftLeft :: ListZipper a -> ListZipper a
shiftLeft (LZ (lh:lt) el r) = LZ lt lh (el : r)
shiftLeft _                 = error "ld"

shiftRight :: ListZipper a -> ListZipper a
shiftRight (LZ l el (rh:rt)) = LZ (el : l) rh rt
shiftRight _                 = error "rd"

insert :: a -> ListZipper a -> ListZipper a
insert el (LZ left _ right) = LZ left el right