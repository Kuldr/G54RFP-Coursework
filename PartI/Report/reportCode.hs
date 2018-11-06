import Data.Numbers.Primes
hamming' :: Integral a => [a]
hamming' = filter (\n -> all (`elem` [2,3,5]) $ primeFactors n) [1..]

hamming :: Integral a => [a]
hamming = 1 : (merge hamming5 $ merge hamming2 hamming3)
            where hamming2 = map (2*) hamming
                  hamming3 = map (3*) hamming
                  hamming5 = map (5*) hamming

evalCell :: Sheet Double -> Exp -> Double
evalCell s (Sum r1 r2) = sum [ evalCell s (Ref r) | r <- range (r1,r2) ]
evalCell s (Avg r1 r2) = evalCell s (Sum r1 r2)
                            / fromIntegral(length $ range (r1,r2))

drop :: Int -> RList a -> RList a
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
