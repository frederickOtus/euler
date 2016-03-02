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
    | otherwise = (0, handMax hand, handMax hand)
