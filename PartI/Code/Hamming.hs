-- hamming20 is the first 20 hamming numbers
hamming20 :: Num a => [a]
hamming20 = [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]

-- hamming generates an infinite list of hamming numbers
hamming :: Num a => [a]
hamming = []

-- hammingNext takes in a number n and returns the list of the hamming numbers directly generated from n and n itself
hammingNext :: Num a => a -> [a]
hammingNext n = [n, x, y, z]
                    where x = 2*n
                          y = 3*n
                          z = 5*n

-- merge takes in 2 lists of the same type and sorts them in asscending order with no duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) | x == y = x : merge xs ys
                            | x <  y = x : merge xs yys
                            | x >  y = y : merge xxs ys
merge xxs        []                  = xxs
merge []          yys                = yys

checkHamming :: Bool
checkHamming = hamming20 == (take 20 hamming)
