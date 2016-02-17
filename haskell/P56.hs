solve :: Integer
solve = maximum nums

nums = [ digSum (a ^ b) | a <- [1..99], b <- [1..99] ]

digSum n
    | n < 10 = n
    | otherwise = (mod n 10) + digSum (quot n 10)

main = print $ show $ solve
