solve ::  Int
solve = sum $ map numSols [1 .. 100]

choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

firstGTM :: Int -> Int
firstGTM n = rFirstGTM n 1

rFirstGTM :: Int -> Int -> Int
rFirstGTM n r
    | tots > 1000000 = r
    | r >= half = 0
    | otherwise = rFirstGTM n (r + 1)
    where tots = choose n r
          half = quot n 2

numSols :: Int -> Int
numSols n
    | fst == 0 = 0
    | otherwise = num
    where fst = firstGTM n
          num = n - (fst * 2) + 1

greaterThanMil :: Int -> Bool
greaterThanMil n =
    let x = quot n 2
    in (<) 1000000 $ choose n x

main :: IO()
main = print solve
