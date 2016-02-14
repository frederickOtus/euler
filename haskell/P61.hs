import Data.List

numOps [] = []
numOps (op:ops) = ns : numOps ops
    where ns = takeWhile (<10000) $ filter (> 999) $ map (truncate . op) [ 1 .. ]

nums = numOps [ (\n -> n*(n+1)/2), (\n -> n*n), (\n -> n*(3*n-1)/2), (\n -> n*(2*n-1)), (\n -> n*(5*n-3)/2), (\n -> n*(3*n-2)) ]


initPrefs = map (\n -> [n]) (nums !! 0)
initSilos = drop 1 nums

siloPairings prefs silo = foldl (++) [] $ map spairs prefs
    where spairs pref = [ s:pref | s <- pPairs (head pref) silo ]

pPairs n ns = filter t ns
    where t = \i -> (quot i 100) == (mod n 100) --4 digit numbers, the last 2 digs of first are first 2 of second

allbut = \l n -> (take n l) ++ (drop (n + 1) l)

proc p [] = p
proc p s = foldl (++) [] (map nextIter sets)
    where sets = [ (s !! n, allbut s n) | n <- [ 0 .. (length s) - 1] ]
          nextIter (st,sts) = proc (siloPairings p st) sts

solve = sum $ nums !! 0
    where f = \l -> (mod (head l) 100) == (quot (last l) 100)
          nums = filter f $ proc initPrefs initSilos

main :: IO()
main = print $ show $ solve
