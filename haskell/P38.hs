import Data.List

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith a b = foldr (&&) True $ zipWith (\x y ->  x == y) a b

testProd seq val = testProd' seq val 1

testProd' :: String -> Int -> Int -> Bool
testProd' [] _ i = i > 1
testProd' seq t i
    | not vlength = False
    | startsWith seq st = testProd' (drop (length st) seq) t (i + 1)
    | otherwise = False 
    where   st = show (i * t)
            vlength = (length seq) >= (length st)

validSeq :: String -> Bool
validSeq str = foldr (||) False sols
    where sols = map f [1 .. 5]
          f = \n -> testProd str (read $ take n str)

possibles = reverse $ sort $ permutations "987654321"

solve = head $ filter validSeq possibles
