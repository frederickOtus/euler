pspace = [ (a, b) | a <- [10 .. 99], b <- [10 .. 99], not (a `mod` 10 ==  0 && b `mod` 10 == 0), b > a]
fspace = filter validPair pspace

solve = quot (snd frac) gd
    where frac = foldr (\(a,b) (c,d) -> (a*c, b*d)) (1,1) fspace
          gd = gcd (fst frac) (snd frac)

validPair (a,b) = p1 || p2 || p3 || p4
    where a1 = quot a 10
          a2 = mod a 10
          b1 = quot b 10
          b2 = mod b 10
          p1 = if a1 == b1 then eqFrac (a,b) (a2,b2)
                else False
          p2 = if a1 == b2 then eqFrac (a,b) (a2,b1)
                else False
          p3 = if a2 == b1 then eqFrac (a,b) (a1,b2)
                else False
          p4 = if a2 == b2 then eqFrac (a,b) (a1,b1)
                else False

eqFrac (a,b) (c,d) = (quot a gcd1) == (quot c gcd2) && (quot b gcd1) == (quot d  gcd2)
    where gcd1 = gcd a b
          gcd2 = gcd c d
