import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readIn solve (head args)

solve :: Int -> String
solve x = show $ sum $ filter filt [1..x]

filt :: Int -> Bool
filt x = (&&) (isPalindrome x) (isBinaryPalindrome x)

isBinaryPalindrome :: Int -> Bool
isBinaryPalindrome x = bstr == reverse bstr
    where bstr = toBin x

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (helper n)

helper :: Int -> [Int]
helper 0 = []
helper n = (n `mod` 2) : helper (n `div` 2)

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

readIn :: (Int -> String) -> String -> String
readIn fn x
    | Prelude.null r_out = "nope"
    | otherwise = fn $ fst $ head r_out
    where 
        r_out = reads x :: [(Int, String)]
