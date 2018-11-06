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
