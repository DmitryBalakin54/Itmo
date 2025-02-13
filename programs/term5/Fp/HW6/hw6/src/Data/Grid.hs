module Data.Grid
  ( Grid(..)
  , insertGrid
  , shifts
  ) where

import Control.Comonad (Comonad(..))
import Data.ListZipper (ListZipper(..), insert, shift, shiftLeft, shiftRight)

newtype Grid a = Grid {unGrid :: ListZipper (ListZipper a)}

instance Functor Grid where
  fmap f (Grid g) = Grid (fmap (fmap f) g)

moveU :: Grid a -> Grid a
moveU (Grid g) = Grid (shiftLeft g)

moveD :: Grid a -> Grid a
moveD (Grid g) = Grid (shiftRight g)

moveL :: Grid a -> Grid a
moveL (Grid g) = Grid (fmap shiftLeft g)

moveR :: Grid a -> Grid a
moveR (Grid g) = Grid (fmap shiftRight g)

shifts :: [Grid a -> Grid a]
shifts = [moveL, moveU, moveR, moveD]

insertGrid :: a -> Grid a -> Grid a
insertGrid el (Grid g) = Grid (insert cur g)
  where
    cur = insert el (extract g)

r :: Grid a -> ListZipper (Grid a)
r = shift moveL moveR

col :: Grid a -> ListZipper (Grid a)
col = shift moveU moveD

instance Comonad Grid where
  extract (Grid g) = extract (extract g)

  duplicate g = Grid (fmap r (col g))
