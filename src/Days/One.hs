module Days.One where

file = "src/Days/input/one.txt"

run = do
    content <- readFile file
    let l = lines content
    print $ calcFuels $ toInts l
    print $ calcFuels' $ toInts l

calcFuels :: [Int] -> Int
calcFuels xs = sum $ map (calcFuel) xs

calcFuels' :: [Int] -> Int
calcFuels' xs = sum $ map (calcFuelRec) xs

calcFuel :: Int -> Int
calcFuel x = (x `div` 3) - 2

calcFuelRec :: Int -> Int
calcFuelRec x
    | res <= 0 = 0
    | otherwise = res + calcFuelRec res
    where res = calcFuel x

toInts :: [String] -> [Int]
toInts = map read
