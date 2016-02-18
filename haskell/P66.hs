
ds = filter nf [ 1 .. 1000 ] 
    where nf = not . isSquare

isSquare :: Int -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) 
                 in x'*x' == x

testND d n = (&&) isdiv $ isSquare $ quot (n * n - 1) d
    where isdiv = mod (n * n - 1) d == 0 && d <= n * n

diop d = head $ filter (testND d) [ 1 .. ]

solve = maximum $ map diop ds
