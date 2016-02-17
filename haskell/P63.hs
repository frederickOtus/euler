solve = length sols
    where sols = foldl (++) [] $ takeWhile ((>0) . length) $ map nDigs [1..]

nDigs n = takeWhile (<mx) $ filter (>mn) seq
    where seq = map (^n) [1..]
          mx = 10 ^ n 
          mn = 10 ^ (n - 1) - 1

main = print $ show $ solve
