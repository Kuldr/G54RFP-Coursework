merge xxs@(x:xs) yys@(y:ys) | x == y = x : merge xs ys
                            | x <  y = x : merge xs yys
                            | x >  y = y : merge xxs ys
