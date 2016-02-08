solve ::  Int
solve = rsolve 125874

rsolve :: Int -> Int
rsolve n
    | nIsValid n = n
    | otherwise = rsolve $ n + 1

modlist :: Int -> [Int] -> [Int]
modlist i l
    | i > 9 || i < 0 = l
    | otherwise = (take i l) ++ [ (+) 1 (l !! i) ] ++ (drop (i + 1) l)

rlistify :: Int -> [Int] -> [Int]
rlistify n l
    | n < 9 = modlist n l
    | otherwise = rlistify (quot n 10) $ modlist (mod n 10) l

listify :: Int -> [Int]
listify n = rlistify n $ take 10 $ repeat 0

comp :: [Int] -> [Int] -> Bool
comp [] [] = True
comp xs [] = False
comp [] xs = False
comp (x:xs) (y:ys)
    | x == y = comp xs ys
    | otherwise = False

nIsValid :: Int -> Bool
nIsValid n = and $ map c $ map listify $ map ((*) n) [2 .. 6 ]
    where c = comp $ listify n

main :: IO()
main = print solve
