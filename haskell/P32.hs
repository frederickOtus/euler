import Data.List


solve = sum $ nub validProds

validProds = foldl (++) [] $ map prodForComb $ permutations [1 .. 9]

prodForComb :: [Int] -> [Int]
prodForComb c = [ digsToNum $ drop (a + b) c | a <- [1 .. 8], b <- [1 .. 8], a + b < 9, testNum c a b ]

testNum :: [Int] -> Int -> Int -> Bool
testNum xs a b = m1 * m2 == prod
    where m1 = digsToNum $ take a xs
          m2 = digsToNum $ take b $ drop a xs
          prod = digsToNum $ drop (a + b) xs

digsToNum :: [Int] -> Int
digsToNum [] = 0
digsToNum (x:xs) = x * 10 ^ (length xs) + digsToNum (xs)
