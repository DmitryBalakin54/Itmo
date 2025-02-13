module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int

data Tree a 
    = Branch Meta (Tree a) a (Tree a) 
    | Leaf


-- Show for tests
-- instance Show Meta where
--     show (M size depth) = "M{" ++ show size ++ ", " ++ show depth ++ "}"
-- instance Show a => Show (Tree a) where
--     show (Branch meta left value right) =
--         "B " ++ show meta ++ " (" ++ show left ++ ") " ++ show value ++ " (" ++ show right ++ ")"
--     show Leaf = "Leaf"


tsize :: Tree a -> Int
tsize Leaf                    = 0
tsize (Branch (M sz _) _ _ _) = sz


tdepth :: Tree a -> Int
tdepth Leaf                    = 0
tdepth (Branch (M _ dp) _ _ _) = dp


-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember v (Branch _ l val r)
    | v == val   = True
    | v < val    = tmember v l
    | otherwise  = tmember v r


-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
--
--Old version
-- tinsert :: Ord a => a -> Tree a -> Tree a
-- tinsert v Leaf = Branch (M 1 1) Leaf v Leaf
-- tinsert v (Branch (M sz dp) l val r)
--   | tmember v (Branch (M sz dp) l val r) = Branch (M sz dp) l val r
--   | v < val  = let newL = tinsert v l
--                    newSz = 1 + sz
--                    newDp = max dp (tdepth newL + 1)
--                in Branch (M newSz newDp) newL val r
--   | otherwise = let newR = tinsert v r
--                     newSz = 1 + sz
--                     newDp = max dp (tdepth newR + 1)
--                 in Branch (M newSz newDp) l val newR

balance :: Tree a -> Int
balance Leaf             = 0
balance (Branch _ l _ r) = tdepth l - tdepth r

newSize :: Tree a -> Tree a -> Int
newSize l r = tsize l + tsize r + 1

newDepth :: Tree a -> Tree a -> Int
newDepth l r = max (tdepth l) (tdepth r) + 1 

rotateR :: Tree a -> Tree a
rotateR (Branch (M sz _) (Branch _ ll lv lr) v r) = Branch (M sz newDp) ll lv newR
    where
        newDp = newDepth ll newR
        newR  = Branch (M (newSize lr r) (newDepth lr r)) lr v r
rotateR _ = Leaf


rotateL :: Tree a -> Tree a
rotateL (Branch (M sz _) l v (Branch _ rl rv rr)) = Branch (M sz newDp) newL rv rr
  where
    newDp = newDepth newL rr
    newL  = Branch (M (newSize l rl) (newDepth l rl)) l v rl
rotateL _ = Leaf


rotateRL :: Tree a -> Tree a
rotateRL (Branch (M sz _) l v r) = rotateR (Branch meta rotL v r)
  where
    meta = M sz (newDepth rotL r)
    rotL = rotateL l
rotateRL _ = Leaf


rotateLR :: Tree a -> Tree a
rotateLR (Branch (M sz _) l v r) = rotateL (Branch meta l v rotR)
    where
        meta = M sz (newDepth rotR l)
        rotR = rotateR r
rotateLR _ = Leaf


tinsert :: Ord a => a -> Tree a -> Tree a
tinsert v Leaf = Branch (M 1 1) Leaf v Leaf
tinsert v t@(Branch _ l val r)
    | v == val  = t
    | v < val   = let newL       = tinsert v l
                      newSz      = newSize newL r
                      newDp      = newDepth newL r
                      balancedL  = balanceTree (Branch (M newSz newDp) newL val r)
                  in balancedL
    | otherwise = let newR      = tinsert v r
                      newSz     = newSize l newR
                      newDp     = newDepth l newR
                      balancedR = balanceTree (Branch (M newSz newDp) l val newR)
                  in balancedR


balanceTree :: Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree t@(Branch _ l _ r)
    | balance t > 1  = if balance l < 0
                            then rotateRL t
                            else rotateR t
    | balance t < -1 = if balance r > 0
                            then rotateLR t
                            else rotateL t
    | otherwise      = t


-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf


