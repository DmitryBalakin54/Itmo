module HW2.T3
  ( epart
  , mcat
  ) where
import Data.Foldable (Foldable(fold))

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toMonoidPair
  where
    toMonoidPair (Left a)  = (a, mempty)
    toMonoidPair (Right b) = (mempty, b)
