data Ivl = Ivl Double Double deriving (Show, Eq)


instance Num Ivl where
    (Ivl xl xu) + (Ivl yl yu) = Ivl (xl+yl) (xu+yu)
    (Ivl xl xu) - (Ivl yl yu) = Ivl (xl-yu) (xu-yl)
    (Ivl xl xu) * (Ivl yl yu) = Ivl (minimum ls) (maximum ls)
                                    where ls = [ x*y | x <- [xl, xu], y <- [yl, yu] ]
    abs (Ivl xl xu) = Ivl 0 (abs (xl - xu))
    fromInteger n = Ivl (fromIntegral n) (fromIntegral n)
    signum i = Ivl (signum x) (signum x) where (Ivl x _) = abs i

instance Fractional Ivl where
    (Ivl xl xu) / (Ivl yl yu) = Ivl (minimum ls) (maximum ls)
                                    where ls = [ x/y | x <- [xl, xu], y <- [yl, yu] ]
    fromRational n = Ivl (fromRational n) (fromRational n)
    recip i = 1 / i

(+/-) :: Double -> Double -> Ivl
(+/-) n x = Ivl (n-x) (n+x)

interval1 = Ivl 1.0 1.0

--Testing-----------------------------------------------------------------------
testAdd1 = (Ivl 1 2) + (Ivl 1 2) == Ivl 2 4
testAdd2 = (Ivl (-3) 1) + (Ivl 1 2) == Ivl (-2) 3
testAdd3 = (Ivl (-3) (-1)) + (Ivl 1 2) == Ivl (-2) 1

testAbs1 = abs (Ivl (-4) (-2)) == Ivl 0.0 2.0
testAbs2 = abs (Ivl (-2) (2)) == Ivl 0.0 4.0
testAbs3 = abs (Ivl (2) (4)) == Ivl 0.0 2.0

testAll = and [testAdd1, testAdd2, testAdd3, testAbs1, testAbs2, testAbs3]
