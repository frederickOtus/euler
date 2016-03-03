import Data.List

type Card = (Int, Int) --first int is val, second is suit

sameSuit ((c,s):hand) = all (\(_,s')-> s' == s) hand
isSeq a@((c,s):_) = and $ zipWith (==) [c..] (map fst a)

handMax :: [Card] -> Int
handMax hand = maximum (map fst hand)

rank :: [Card] -> (Int, Int, Int)
rank hand@((c,s):h)
    | isSeq hand && sameSuit hand && c == 10 = (9, 14, 14)
    | isSeq hand && sameSuit hand = (9, handMax hand, handMax hand)
    | sameSuit hand = (5, handMax hand, handMax hand)
    | isSeq hand = (4, handMax hand, handMax hand)
    | length ms == length hand = (0, handMax hand, handMax hand)
    where ms = smatches hand

smatches :: [Card] -> [(Int, Int)]
smatches hand = sortBy (flip cmp) ms
    where ms = matches hand
          cmp (ps,vls) (ps',vls') = if ps == ps' then compare vls vls' else compare ps ps'

matches :: [Card] -> [(Int, Int)] --size / card
matches [] = []
matches all@((c,s):cs) = (cnt,c) : matches rest
    where cnt = length $ takeWhile (\(c',_) -> c == c') all
          rest = dropWhile (\(c',_) -> c == c') all

