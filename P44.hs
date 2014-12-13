
genPent :: Integer -> Integer
genPent a = floor $ n * (3 * n - 1 ) / 2
    where n = fromIntegral a

isPent :: Integer -> Bool
isPent n = n == genPent (fst nPrime) || n == genPent (snd nPrime)
    where nPrime = qEquation (3.0, (-1.0), (- 2.0 * (fromIntegral n)))

qEquation :: (Float, Float, Float) -> (Integer, Integer)
qEquation (a, b, c) = (floor x1, floor x2)
    where
        x1 = e + sqrt d / (2 * a)
        x2 = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)
