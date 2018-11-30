seriesK :: Int -> [Rational]
seriesK k = let r_k = toRational k in knot (r_k, 0)

knot :: (Rational, Int) -> [Rational]
knot (x, s)  = 1 / y : result_list
    where y = x ^ s
          result_list = knot (x, s + 1)