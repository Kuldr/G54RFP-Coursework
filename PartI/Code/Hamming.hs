import Data.Numbers.Primes

-- hamming generates an infinite list of hamming numbers
hamming :: Integral a => [a]
hamming = 1 : (merge hamming5 $ merge hamming2 hamming3)
            where hamming2 = map (2*) hamming
                  hamming3 = map (3*) hamming
                  hamming5 = map (5*) hamming

-- merge takes in 2 lists of the same type and sorts them in asscending order with no duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) | x == y = x : merge xs ys
                            | x <  y = x : merge xs yys
                            | x >  y = y : merge xxs ys

-- hamming' generates an infinite list of hamming numbers by filtering numbers
--      with the prime factors 2,3,5
hamming' :: Integral a => [a]
hamming' = filter (\n -> all (`elem` [2,3,5]) $ primeFactors n) [1..]
