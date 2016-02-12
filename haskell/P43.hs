import Data.List

coll [] = 0
coll (x:xs) = x * 10 ^ (length xs) + (coll xs)

numDivByN :: [Int] -> Int -> Int -> Bool
numDivByN l n d 
    | (head l) == 0 = False
    | otherwise = (mod val d) == 0
    where nums = take 3 $ drop (n - 1) l
          val = coll nums

valid :: [Int] -> Bool
valid l = a && b && c && d && e && f && g
    where a = numDivByN l 2 2
          b = numDivByN l 3 3
          c = numDivByN l 4 5
          d = numDivByN l 5 7
          e = numDivByN l 6 11
          f = numDivByN l 7 13
          g = numDivByN l 8 17


solve = sum $ map coll $ filter valid $ permutations [0,1,2,3,4,5,6,7,8,9]

main :: IO()
main = print solve
