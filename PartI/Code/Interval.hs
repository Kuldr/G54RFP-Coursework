data Ivl = Ivl Double Double deriving Show

instance Num Ivl where
    (Ivl xl xu) + (Ivl yl yu) = Ivl (xl+yl) (xu+yu)
    (Ivl xl xu) - (Ivl yl yu) = Ivl (xl-yu) (xu-yl)
    (Ivl xl xu) * (Ivl yl yu) = Ivl (minimum ls) (maximum ls)
                                    where ls = [ x*y | x <- [xl, xu], y <- [yl, yu] ]
    abs (Ivl xl xu) = Ivl ((xl + xu) / 2) ((xl + xu) / 2)
                    -- | (xl >= 0) && (xu >= 0) = Ivl xl xu
                    -- | (xl < 0) && (xu < 0) = Ivl (-xl) (-xu)
                    -- | (xl < 0) && (xu > 0) = Ivl 0 (max (abs xl) (abs xu))
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
