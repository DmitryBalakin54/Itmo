module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)


splitOn_:: (Eq a) => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOn_ sep el lst@(h :| t) 
  | el == sep  = [] :| toList lst
  | otherwise  = (el : h) :| t

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (splitOn_ sep) ([] :| [])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (h :| [])     = h
joinWith a (h :| (x:xs)) = h ++ [a] ++ joinWith a (x :| xs)
