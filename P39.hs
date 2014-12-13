import System.Environment
import Data.Map.Strict as Map

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readIn solve (head args)

solve :: Int -> String
solve x =
    let mpdata = toMap (Prelude.filter permFilt (prep $ pairs x)) empty
    in show $ largest mpdata (keys mpdata) ((head (keys mpdata)),(mpdata ! (head (keys mpdata))))

largest :: Map Int Int -> [Int] -> (Int,Int) -> Int
largest mp [] (ind,max) = ind
largest mp (x:xs) (ind,mx)
    | perim > mx = largest mp xs (x,perim)
    | otherwise = largest mp xs (ind,mx)
    where perim = mp ! x

toMap :: [(Int, Int, Int)] -> Map Int Int -> Map Int Int
toMap [] mp = mp
toMap ((a,b,c):xs) mp
    | member perim mp = toMap xs $ insert perim ((+) 1 (mp ! perim)) mp
    | otherwise = toMap xs $ insert perim 1 mp
    where perim = a + b + c

permFilt :: (Int, Int, Int) -> Bool
permFilt (a, b, c) = a + b + c < 1001

prep :: [(Int, Int)] -> [(Int, Int, Int)]
prep [] = []
prep lst = Prelude.map (\p -> (fst p, snd p, sp p)) fl
    where fl = Prelude.filter isTriPair lst

sp :: (Int, Int) -> Int
sp (x, y) = truncate $ sqrt (fromIntegral p :: Double)
    where p = (x * x) + (y * y)

pairs :: Int -> [(Int, Int)]
pairs 0 = slice 0
pairs x = slice x ++ pairs (x - 1)

slice :: Int -> [(Int, Int)]
slice x = Prelude.map (\n -> (n, x - n)) $ take (quot (x + 1) 2) [1..]

isTriPair :: (Int, Int) -> Bool
isTriPair (x, y) = isSquare $ (+) (x * x) (y * y)

isSquare :: Int -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

readIn :: (Int -> String) -> String -> String
readIn fn x
    | Prelude.null r_out = "nope"
    | otherwise = fn $ fst $ head r_out
    where 
        r_out = reads x :: [(Int, String)]
