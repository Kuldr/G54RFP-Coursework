-- merge takes in 2 lists of the same type and sorts them in asscending order with no duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys) | x == y = x : merge xs ys
                            | x <  y = x : merge xs yys
                            | x >  y = y : merge xxs ys
merge xxs        []                  = xxs
merge []          yys                = yys
