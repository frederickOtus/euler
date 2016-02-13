import Data.List

tri = [ truncate val | n <- [1 .. 141], let val = n*(n+1)/2, val > 999 && val < 10000 ]
sqr = [ truncate val | n <- [1 .. 141], let val = n*n, val > 999 && val < 10000 ]
pen = [ truncate val | n <- [1 .. 141], let val = n*(3*n-1)/2, val > 999 && val < 10000 ]
hex = [ truncate val | n <- [1 .. 141], let val = n*(2*n-1), val > 999 && val < 10000 ]
hep = [ truncate val | n <- [1 .. 141], let val = n*(5*n-3)/2, val > 999 && val < 10000 ]
oct = [ truncate val | n <- [1 .. 141], let val = n*(3*n-2), val > 999 && val < 10000 ]

nums = [ [t,s,p,h,hp,o] | t <- tri, s <- sqr, p <-pen, h <-hex, hp <- hep, o <-oct ]

isCycle :: [Int] -> Bool
isCycle l@(x:xs) = tCycle (l ++ [x])

tCycle :: [Int] -> Bool
tCycle [] = True
tCycle [n] = True
tCycle (n1:n2:ns) = (a == b) && (tCycle (n2:ns))
    where a = mod n1 100
          b = quot n2 100

testP [] = False
testP (x:xs) = (isCycle x) || (testP xs)

test n = testP $ permutations n

solve = find testP nums

main :: IO()
main = print $ show $ solve
