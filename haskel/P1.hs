

solve :: Int -> Int
solve 0 = 0
solve a
    | mod a 3 == 0 || mod a 5 == 0 = (+) a (solve ((-) a 1))
    | otherwise = solve (a - 1)

main :: IO()
main = print (solve 10)
