
jan y = 31
feb y
    | not div4 || (div100 && not div400) = 28
    | otherwise  = 29
    where div4 = mod y 4 == 0
          div100 = mod y 100 == 0
          div400 = mod y 400 == 0
mar y = 31
apr y = 30
may y = 31
jun y = 30
jul y = 31
aug y = 31
sep y = 30
oct y = 31
nov y = 30
dec y = 31

months = [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]

years s e fd = foldl f (fd, 0) [s .. e]
    where f = \(fd, pys) y -> let (s, yrs) = processYear fd y
                                in (mod s 7, (yearTots yrs) + pys)

processYear start year = foldl f (start, [start]) increments
    where increments = map ($ year) months
          f = \(s, p) i -> (s + i, p ++ [s+i])

yearTots l = sum $ map f (take 12 l)
    where f = \n -> if (mod n 7) == 0 then 1 else 0
