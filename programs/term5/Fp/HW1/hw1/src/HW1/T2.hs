module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural
import Data.Maybe

data N = Z | S N

nplus :: N -> N -> N
nplus Z b     = b
nplus a Z     = a
nplus (S a) b = S $ nplus a b

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult (S a) b = nplus b $ nmult a b

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp a b = case nsub a b of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
nToNum :: Num a => N -> a
--nToNum :: N -> a
nToNum Z     = 0
nToNum (S a) = (+) 1 $ nToNum a

nEven :: N -> Bool
nEven a = case nmod a (S (S Z)) of
  Z -> True
  _ -> False

nOdd :: N -> Bool
nOdd a = not $ nEven a


nsub_ :: N -> N -> N
nsub_ a b = fromMaybe Z (nsub a b)

-- nsub_ :: N -> N -> N
-- nsub_ a b = case nsub a b of
--   Nothing -> Z
--   Just c  -> c

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv a b = case ncmp a b of 
  LT -> Z
  EQ -> S Z
  GT -> nplus (S Z) (ndiv (nsub_ a b) b)

nmod :: N -> N -> N
nmod _ Z = undefined
nmod a b = case ncmp a b of
  GT -> nmod (nsub_ a b) b
  EQ -> Z
  LT -> a
