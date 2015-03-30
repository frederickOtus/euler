import System.Environment
import Data.Map.Strict as Map

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readIn solve (head args)

readIn :: (Int -> String) -> String -> String
readIn fn x
    | Prelude.null r_out = "nope"
    | otherwise = fn $ fst $ head r_out
    where 
        r_out = reads x :: [(Int, String)]

solve :: Int -> String
solve limit = show $ fibsum 1 1 0 limit

fibsum :: Int -> Int -> Int -> Int -> Int
fibsum nm1 nm2 sum limit
    | limit < (nm1 + nm2) = sum
    | (nm1 + nm2) `mod` 2 /= 0 = fibsum (nm2 + nm1) nm1 sum limit 
    | otherwise = fibsum (nm2 + nm1) nm1 (nm1 + nm2 + sum) limit
