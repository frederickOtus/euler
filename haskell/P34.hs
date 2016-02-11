--solve ::  Int
--solve = sum $ map numSols [1 .. 100]

fact 0 = 1
fact 1 = 1
fact n = n * (fact (n - 1))

listify :: Int  -> [Int]
listify n 
    | n < 9 = [n]
    | otherwise =  (mod n 10) : (listify (quot n 10))

isCurious :: Int -> Bool
isCurious n = n == (sum $ map fact $ listify n)

maxNum = 9999999

solve = sum $ filter isCurious [ 3 .. maxNum ]

main :: IO()
main = print solve
