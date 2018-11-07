-- Skew Binary Random-Access List

module SBRAL where

import Prelude hiding (head, tail, lookup, drop)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)
type RList a = [(Int, Tree a)]

testDrop :: Int -> Bool
testDrop i = list1 == drop i list2
                where list1 = last $ take i (iterate (cons 1) empty)
                      list2 = last $ take (2*i) (iterate (cons 1) empty)

testDrops :: [Bool]
testDrops = [ testDrop i | i <- [1..] ]

testDropsToN :: Int -> Bool
testDropsToN n = and $ take n testDrops

drop :: Int -> RList a -> RList a
drop _ [] = []
drop i ((w, t) : wts) | i < w     = dropTree i w t ++ wts
                      | otherwise = drop (i - w) wts

dropTree :: Int -> Int -> Tree a -> RList a
dropTree i _ (Leaf x) | i /= 0    = []
                      | otherwise = [(1, Leaf x)]
dropTree i w (Node t1 x t2) | i == 0    = [(w, (Node t1 x t2))]
                            | i <= w'   = dropTree (i - 1) w' t1 ++ [(w', t2)]
                            | otherwise = dropTree (i - w' - 1) w' t2
                                    where
                                        w' = w `div` 2

empty :: RList a
empty = []


isEmpty :: RList a -> Bool
isEmpty ts = null ts


cons :: a -> RList a -> RList a
cons x ((w1, t1) : (w2, t2) : wts) | w1 == w2 = (w1 * 2 + 1, Node t1 x t2) : wts
cons x wts = ((1, Leaf x) : wts)


head :: RList a -> a
head ((_, Leaf x)     : _) = x
head ((_, Node _ x _) : _) = x


tail :: RList a -> RList a
tail ((_, Leaf _): wts)        = wts
tail ((w, Node t1 _ t2) : wts) = (w', t1) : (w', t2) : wts
    where
        w' = w `div` 2


lookup :: Int -> RList a -> a
lookup i ((w, t) : wts) | i < w      = lookupTree i w t
                        | otherwise  = lookup (i - w) wts


lookupTree :: Int -> Int -> Tree a -> a
lookupTree _ _ (Leaf x) = x
lookupTree i w (Node t1 x t2)
    | i == 0    = x
    | i < w'   = lookupTree (i - 1) w' t1
    | otherwise = lookupTree (i - w' - 1) w' t2
    where
        w' = w `div` 2

update :: Int -> a -> RList a -> RList a
update i x (wt@(w, t) : wts) | i < w      = (w, updateTree i x w t) : wts
                             | otherwise  = wt : update (i - w) x wts


updateTree :: Int -> a -> Int -> Tree a -> Tree a
updateTree _ x _ (Leaf _) = Leaf x
updateTree i x w (Node t1 y t2)
    | i == 0    = (Node t1 x t2)
    | i <= w'   = Node (updateTree (i - 1) x w' t1) y t2
    | otherwise = Node t1 y (updateTree (i - w' - 1) x w' t2)
    where
        w' = w `div` 2

list3 :: RList Int
list3 = [(3, Node (Leaf 1) 2 (Leaf 3)) ]

list2 :: RList Int
list2 = [(1, Leaf 1), (1, Leaf 2), (7, Node (Node (Leaf 3) 4 (Leaf 5)) 6 (Node (Leaf 7) 8 (Leaf 9)))]

list4 :: RList Int
list4 = [(1,Leaf 1),(1,Leaf 2),(7,Node (Node (Leaf 5) 4 (Leaf 6)) 3 (Node (Leaf 8) 7 (Leaf 9)))]
