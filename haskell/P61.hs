import Data.List

numOps [] = []
numOps (op:ops) = ns : numOps ops
    where ns = takeWhile (<10000) $ filter (> 999) $ map (truncate . op) [ 1 .. ]

nums = numOps [ (\n -> n*(n+1)/2), (\n -> n*n), (\n -> n*(3*n-1)/2), (\n -> n*(2*n-1)), (\n -> n*(5*n-3)/2), (\n -> n*(3*n-2)) ]

buildCycle n [] = [n]
--buildCycle n (x) =

pPairs n ns = filter t ns
    where t = \i -> (quot i 100) == (mod n 100)


--solve = find testP nums

--main :: IO()
--main = print $ show $ solve
