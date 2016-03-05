import Data.List
import Data.Char

type Card = (Int, Char) --first int is val, second is suit

sameSuit ((c,s):hand) = all (\(_,s')-> s' == s) hand
isSeq a@((c,s):_) = and $ zipWith (==) [c..] (map fst a)

handMax :: [Card] -> Int
handMax hand = maximum (map fst hand)

rties :: [Int] -> [Int] -> Ordering
rties [] [] = EQ
rties [] _ = EQ
rties _ [] = EQ
rties (a:as) (b:bs) 
    | a > b = GT
    | b < a = LT
    | a == b = rties as bs
rties [a] [b] = a `compare` b

leftWins :: ([Card],[Card]) -> Bool
leftWins (h1,h2)
    | r1 > r1 = True
    | r1 == r2 && tie == GT = True
    | r1 == r2 && tie == EQ && handRank h1 h2 = True
    | otherwise = False
    where (r1,t1) = rank h1
          (r2,t2) = rank h2    
          tie = rties t1 t2

handRank [] [] = False
handRank ((v1,_):h1) ((v2,_):h2)
    | v1 > v2 = True
    | v1 < v2 = False
    | otherwise = handRank h1 h2

{-
 - 0 High Card
 - 1 One Pair
 - 2 Two Pair
 - 3 Three of a kind
 - 4 Straight
 - 5 Flush
 - 6 Full House
 - 7 Four of a Kind
 - 8 Straight Flush
 - 9 Royal Flush
 -}
rank :: [Card] -> (Int, [Int]) --return rank and first tie break
rank hand@((c,s):h)
    | isSeq hand && sameSuit hand && c == 10 = (9, []) --royal flush
    | isSeq hand && sameSuit hand = (8, []) --straight flush
    | sameSuit hand = (5, []) -- flush
    | isSeq hand = (4, []) --straight
    | length ms == length hand = (0, []) --high card
    | otherwise = rankMatches ms
    where ms = smatches hand

rankMatches :: [(Int, Int)] -> (Int, [Int])
rankMatches ((4, h):_) = (7, [h]) --4 of a kind
rankMatches [(3, h1),(2,h2)] = (6, [h1,h2]) --full house
rankMatches ((3, h1):_) = (3, [h1]) --two pair
rankMatches ((2, h1):(2,h2):_) = (2, [max h1 h2, min h1 h2]) --two pair
rankMatches ((2, h):_) = (1, [h]) --two pair

smatches :: [Card] -> [(Int, Int)]
smatches hand = sortBy (flip cmp) ms
    where ms = matches hand
          cmp (ps,vls) (ps',vls') = if ps == ps' then compare vls vls' else compare ps ps'

matches :: [Card] -> [(Int, Int)] --size / card
matches [] = []
matches all@((c,s):cs) = (cnt,c) : matches rest
    where cnt = length $ takeWhile (\(c',_) -> c == c') all
          rest = dropWhile (\(c',_) -> c == c') all

parseHand :: String -> [Card]
parseHand s = map f ws
    where f ([v,s]) = (c2Dig v, s)
          ws = words s

cards2Hands :: [Card] -> ([Card], [Card])
cards2Hands cs = (sorter h1, sorter h2)
    where h1 = take 5 cs
          h2 = drop 5 cs
          cmp (v1,_) (v2,_) = v1 `compare` v2
          sorter = sortBy (flip cmp)

c2Dig c
    | c `elem` ['2' .. '9'] = digitToInt c
    | c == 'T' = 10
    | c == 'J' = 11
    | c == 'Q' = 12
    | c == 'K' = 13
    | c == 'A' = 14

t1 = "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
t2 = "5D 8C 9S JS AC 2C 5C 7D 8S QH"
t3 = "2D 9C AS AH AC 3D 6D 7D TD QD"
t4 = "4D 6S 9H QH QC 3D 6D 7H QD QS"
t5 = "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"

ts = [t1,t2,t3,t4,t5]
test = solve ts

solve = length . filter leftWins . map (cards2Hands . parseHand)

main =  fmap (show . solve . lines) fstring >>= putStrLn
    where fstring = readFile "p054_poker.txt"
          toHands = cards2Hands . parseHand
